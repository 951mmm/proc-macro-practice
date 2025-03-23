use std::process::id;

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Expr, Field, Fields, FieldsNamed,
    FnArg, Ident, Item, ItemStruct, Type, TypePath,
};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut ast = parse_macro_input!(input as Item);

    match resolve_bitfield(&mut ast) {
        Ok(token_stream) => quote! {
            #ast
            #token_stream
        },
        Err(e) => e.to_compile_error(),
    }
    .into()
}

fn resolve_bitfield(ast: &mut Item) -> syn::Result<proc_macro2::TokenStream> {
    match ast {
        Item::Struct(ItemStruct {
            attrs,
            fields,
            ident,
            ..
        }) => {
            resolve_attr(attrs);

            let impl_fn = resolve_fields(fields)?;
            // println!("#impl: {}", impl_fn);

            Ok(quote! {
                impl #ident {
                    #impl_fn
                }
            })
        }
        _ => return Err(syn::Error::new(ast.span(), "expected struct")),
    }
}

// add '#[repr_c]' attr
fn resolve_attr(attrs: &mut Vec<Attribute>) {
    debug_assert_eq!(attrs.len(), 0);
    attrs.push(parse_quote! {#[repr(C)]});
}

const BYTE_LEN: u16 = 8;

fn resolve_fields(fields: &mut Fields) -> syn::Result<proc_macro2::TokenStream> {
    let getters = get_getters(fields)?;
    let setters = get_setters(fields)?;

    let mut size_sum = 0;
    for field in fields.iter_mut() {
        let size = get_size(&field.ty)?;
        size_sum += size as u16;
    }

    let byte_sum = (size_sum / BYTE_LEN) as usize;
    let init_fn = get_init_fn(byte_sum);
    let new_fields: FieldsNamed = parse_quote! {
        {
            data: [u8; #byte_sum],
        }
    };
    *fields = new_fields.into();

    Ok(quote! {
        #init_fn
        #getters
        #setters
    })
}

fn get_init_fn(size_sum: usize) -> proc_macro2::TokenStream {
    let arr = vec![0u8];
    let arr = arr.repeat(size_sum);
    quote! {
        pub fn new() -> Self {
            Self {
                data: [#(#arr,)*]
            }
        }
    }
}

macro_rules! mask {
    ($len:expr) => {
        ((1_u16 << $len) - 1)
    };
}

fn get_getters(fields: &Fields) -> syn::Result<proc_macro2::TokenStream> {
    let mut size_sum: u16 = 0;
    let mut getters = vec![];
    for field in fields {
        let ident = field.ident.clone().unwrap();
        let fn_ident = Ident::new(&format!("get_{}", ident.to_string()), ident.span());
        let mut size = get_size(&field.ty)?;
        let byte_offset = size_sum / BYTE_LEN;
        let byte_offset_usize = byte_offset as usize;
        let offset = size_sum % BYTE_LEN;
        let offset_u8 = offset as u8;

        // |++++++++|+++++++|
        // |  |    |
        // |  |size|
        // | offset
        // byte offset
        let mut stmts = vec![];
        let mut offset_sum: u64 = 0;
        if offset != 0 {
            let rest_size = BYTE_LEN - offset;
            let dt = if size >= rest_size as u8 {
                rest_size as u8
            } else {
                size
            };
            let mask = mask!(dt) as u8;
            stmts.push(quote! {
                let mut result = 0_u64;
                result += ((self.data[#byte_offset_usize]>>#offset_u8) & #mask) as u64;
            });

            size -= dt;
            offset_sum += dt as u64;
        } else {
            stmts.push(quote! {
                let mut result = 0_u64;
            });
        }

        while size >= BYTE_LEN as u8 {
            let byte_offset_usize = ((size_sum + offset_sum as u16) / BYTE_LEN) as usize;
            stmts.push(quote! {
                result += (self.data[#byte_offset_usize] as u64)<<#offset_sum;
            });
            size -= BYTE_LEN as u8;
            offset_sum += BYTE_LEN as u64;
        }

        if size != 0 {
            let byte_offset_usize = ((size_sum + offset_sum as u16) / BYTE_LEN) as usize;
            let mask = mask!(size) as u8;
            stmts.push(quote! {
                result += ((self.data[#byte_offset_usize] & #mask) as u64)<<#offset_sum;
            });
            offset_sum += size as u64;
        }
        stmts.push(quote! {
            result
        });
        getters.push(quote! {
            pub fn #fn_ident(&self) -> u64 {
                #(#stmts)*
            }
        });

        size_sum += offset_sum as u16;
    }

    Ok(quote! {
        #(#getters)*
    })
}
fn get_setters(fields: &Fields) -> syn::Result<proc_macro2::TokenStream> {
    let mut size_sum: u16 = 0;
    let mut setters = vec![];

    for field in fields {
        let ident = field.ident.clone().unwrap();
        let fn_ident = Ident::new(&format!("set_{}", ident.to_string()), ident.span());
        let mut size = get_size(&field.ty)?;
        let byte_offset = (size_sum / BYTE_LEN) as usize;
        let offset = size_sum % BYTE_LEN;
        let offset_u8 = offset as u8;

        let mut stmts = vec![];
        let mut offset_sum: u64 = 0;
        let rest_size = BYTE_LEN - offset;
        let dt = if size >= rest_size as u8 {
            rest_size as u8
        } else {
            size
        };

        let mask = mask!(dt) as u64;

        stmts.push(quote! {
            self.set_data(#byte_offset, #offset_u8, #dt, (value&#mask) as u8);
        });

        size -= dt;
        offset_sum += dt as u64;

        while size >= BYTE_LEN as u8 {
            let byte_offset = ((size_sum + offset_sum as u16) / BYTE_LEN) as usize;
            let byte_len_u8 = BYTE_LEN as u8;
            stmts.push(quote! {
                self.set_data(#byte_offset, 0u8, #byte_len_u8, (value>>#offset_sum) as u8);
            });
            offset_sum += BYTE_LEN as u64;
            size -= BYTE_LEN as u8;
        }

        if size != 0 {
            let byte_offset = ((size_sum + offset_sum as u16) / BYTE_LEN) as usize;
            stmts.push(quote! {
                self.set_data(#byte_offset, 0u8, #size, (value>>#offset_sum) as u8);
            });
            offset_sum += size as u64;
        }

        setters.push(quote! {
            pub fn #fn_ident(&mut self, value: u64) {
                #(#stmts)*
            }
        });

        size_sum += offset_sum as u16;
    }

    Ok(quote! {
        fn set_data(&mut self, index: usize, offset: u8, size: u8, value: u8) {
            let mask = ((1_u16 << size as u16) - 1) as u8;
            let offset_mask = mask << offset;
            let clear_mask = !offset_mask;
            let offset_value = value << offset;
            self.data[index] &= clear_mask;
            self.data[index] |= offset_value;
        }
        #(#setters)*
    })
}

fn get_size(ty: &Type) -> syn::Result<u8> {
    if let Type::Path(TypePath { path, .. }) = &ty {
        let seg_last = path.segments.last().unwrap();
        let ident_string = seg_last.ident.to_string();
        let size_lit = ident_string.split("B").collect::<Vec<_>>()[1];
        let size = match size_lit.parse::<u8>() {
            Ok(size) => size,
            Err(_) => {
                return Err(syn::Error::new(
                    path.span(),
                    "ty shound be 'B1, B2, ...B~N'",
                ))
            }
        };

        Ok(size)
    } else {
        Err(syn::Error::new(ty.span(), "field type should be Path-Like"))
    }
}
