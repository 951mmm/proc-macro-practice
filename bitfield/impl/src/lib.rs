use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Expr, Field, Fields, FieldsNamed,
    Ident, Item, ItemStruct, Type, TypePath,
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
    let mut getters = vec![];
    let mut offset_total_stmts = vec![quote! {let mut offset = 0u16;}];
    for field in fields {
        let ident = get_ident(field)?;
        let fn_ident = Ident::new(&format!("get_{}", ident.to_string()), ident.span());
        let ty = &field.ty;

        getters.push(quote! {
            fn #fn_ident(&self) -> u64 {
                #(#offset_total_stmts)*;
                let mut size = <#ty as Specifier>::BITS;
                let byte_size = <Byte as Specifier>::BITS;
                let index = offset / byte_size as u16;
                let offset_inner = offset % byte_size as u16;

                let mut result = 0u64;
                let mut offset_total = 0u64;
                let size_reverse = byte_size - offset_inner as u8;
                if size <= size_reverse {
                    return self.get_data(index as usize, offset_inner as u8, size) as u64;
                }

                result += self.get_data(index as usize, offset_inner as u8, size_reverse) as u64;
                size -= size_reverse;
                offset_total += size_reverse as u64;

                while size >= byte_size {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    result += (self.get_data(index as usize, 0, byte_size) as u64) << offset_total;
                    size -= byte_size;
                    offset_total += byte_size as u64;
                }

                if size > 0 {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    result += (self.get_data(index as usize, 0, size) as u64) << offset_total;
                }

                 result
            }
        });

        offset_total_stmts.push(quote! {
            offset += <#ty as Specifier>::BITS as u16;
        });
    }
    Ok(quote! {
        fn get_data(&self, index: usize, offset: u8, size: u8) -> u8 {
            let byte = self.data[index];
            let byte = byte >> offset;
            let mask = ((1 << size as u16) - 1) as u8;
            byte & mask
        }
        #(#getters)*
    })
}
fn get_setters(fields: &Fields) -> syn::Result<proc_macro2::TokenStream> {
    let mut setters = vec![];
    let mut offset_total_stmts = vec![quote! {let mut offset = 0u16;}];
    for field in fields {
        let ident = get_ident(field)?;
        let fn_ident = Ident::new(&format!("set_{}", ident.to_string()), ident.span());
        let ty = &field.ty;

        setters.push(quote! {
            fn #fn_ident(&mut self, value: u64) {
                #(#offset_total_stmts)*
                let mut size = <#ty as Specifier>::BITS;
                let byte_size = <Byte as Specifier>::BITS;
                let index = offset / byte_size as u16;
                let offset_inner = offset % byte_size as u16;
                let size_reverse = byte_size - offset_inner as u8;

                let mut offset_total = 0u64;
                if size <= size_reverse {
                    self.set_data(index as usize, offset_inner as u8, size, value as u8);
                    return;
                }

                offset_total += size_reverse as u64;
                while(size >= byte_size) {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    self.set_data(index as usize, 0, byte_size, (value >> offset_total) as u8);
                    size -= byte_size;
                    offset_total += byte_size as u64;
                }

                if size > 0 {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    self.set_data(index as usize, 0, size, (value >> offset_total) as u8);
                }
            }
        });
        offset_total_stmts.push(quote! {
            offset += <#ty as Specifier>::BITS as u16;
        });
    }
    Ok(quote! {
        fn set_data(&mut self, index: usize, offset: u8, size: u8, value: u8) {
            let mask = ((1_u16 << size as u16) - 1) as u8;
            let offset_mask = mask << offset;
            let clear_mask = !offset_mask;
            let offset_value = value << offset;
            self.data[index] &= clear_mask;
            self.data[index] |= offset_value & offset_mask;

        }
        #(#setters)*
    })
}

fn get_ident(field: &Field) -> syn::Result<Ident> {
    match field.ident.clone() {
        Some(ident) => Ok(ident),
        None => Err(syn::Error::new(field.span(), "expected named field")),
    }
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
