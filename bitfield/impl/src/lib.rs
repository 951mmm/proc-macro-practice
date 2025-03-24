use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Expr, Field, Fields, FieldsNamed,
    Ident, Item, ItemStruct, LitInt, Type, TypePath,
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

fn resolve_fields(fields: &mut Fields) -> syn::Result<proc_macro2::TokenStream> {
    let getters = get_getters(fields)?;
    let setters = get_setters(fields)?;
    let check_fn = get_check_fn(fields)?;

    let mut total_size_stmts = vec![];
    for field in fields.iter() {
        let ty = &field.ty;
        total_size_stmts.push(quote! {
            <#ty as Specifier>::BITS
        });
    }

    let array_len = quote! {
        ((#(#total_size_stmts)+*) / <Byte as Specifier>::BITS) as usize
    };

    let init_fn = quote! {
        pub fn new() -> Self {
            Self {
                data: [0; #array_len]
            }
        }
    };
    let new_fields: FieldsNamed = parse_quote! {
        {
            data: [u8; #array_len],
        }
    };
    *fields = new_fields.into();

    Ok(quote! {
        #init_fn
        #getters
        #setters
        #check_fn
    })
}

fn get_getters(fields: &Fields) -> syn::Result<proc_macro2::TokenStream> {
    let mut getters = vec![];
    let mut offset_total_stmts = vec![quote! {let mut offset = 0u16;}];
    for field in fields {
        let ident = get_ident(field)?;
        let fn_ident = Ident::new(&format!("get_{}", ident.to_string()), ident.span());
        let ty = &field.ty;
        let uint_type = quote! {<#ty as UintSpecifier>::Uint};

        getters.push(quote! {
            fn #fn_ident(&self) -> #uint_type {
                #(#offset_total_stmts)*;
                let mut size = <#ty as Specifier>::BITS;
                let byte_size = <Byte as Specifier>::BITS;
                let index = offset / byte_size as u16;
                let offset_inner = offset % byte_size as u16;

                let mut result = 0;
                let mut offset_total = 0;
                let size_reverse = byte_size - offset_inner as u8;
                if size <= size_reverse {
                    return self.get_data(index as usize, offset_inner as u8, size) as #uint_type;
                }

                result += self.get_data(index as usize, offset_inner as u8, size_reverse) as #uint_type;
                size -= size_reverse;
                offset_total += size_reverse as #uint_type;

                while size >= byte_size {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    result += (self.get_data(index as usize, 0, byte_size) as #uint_type) << offset_total;
                    size -= byte_size;
                    offset_total += byte_size as #uint_type;
                }

                if size > 0 {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    result += (self.get_data(index as usize, 0, size) as #uint_type) << offset_total;
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
        let uint_type = quote! {<#ty as UintSpecifier>::Uint};

        setters.push(quote! {
            fn #fn_ident(&mut self, value: #uint_type) {
                #(#offset_total_stmts)*
                let mut size = <#ty as Specifier>::BITS;
                let byte_size = <Byte as Specifier>::BITS;
                let index = offset / byte_size as u16;
                let offset_inner = offset % byte_size as u16;
                let size_reverse = byte_size - offset_inner as u8;

                let mut offset_total = 0;
                if size <= size_reverse {
                    self.set_data(index as usize, offset_inner as u8, size, value as u8);
                    return;
                }

                self.set_data(index as usize, offset_inner as u8, size_reverse, value as u8);
                size -= size_reverse;
                offset_total += size_reverse as #uint_type;
                while(size >= byte_size) {
                    let index = (offset + offset_total as u16) / byte_size as u16;
                    self.set_data(index as usize, 0, byte_size, (value >> offset_total) as u8);
                    size -= byte_size;
                    offset_total += byte_size as #uint_type;
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

fn get_check_fn(fields: &Fields) -> syn::Result<proc_macro2::TokenStream> {
    let mut associate_types = vec![];
    for field in fields {
        let ty = &field.ty;
        associate_types.push(quote! {
            <#ty as Mod8Specifier>::Mod8Type
        });
    }

    let mut sum = match associate_types.pop() {
        Some(associate_type) => associate_type,
        None => {
            return Err(syn::Error::new(
                Span::call_site(),
                "fields should not be empty",
            ))
        }
    };

    while let Some(rhs) = associate_types.pop() {
        sum = mod8_type_add(sum, rhs);
    }

    let check_fn = quote! {
        #[allow(unused)]
        fn _unused_check() {
            fn assert<N: TotalSizeIsMultipleOfEightBits>() {}
            assert::<#sum>();
        }
    };
    Ok(check_fn)
}

fn mod8_type_add(
    lhs: proc_macro2::TokenStream,
    rhs: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    quote! {
        <#lhs as std::ops::Add<#rhs>>::Output
    }
}

#[proc_macro]
pub fn generate_bits(_: TokenStream) -> TokenStream {
    let mut bits = vec![];
    for i in 0..63 {
        let ident = Ident::new(&format!("B{}", i.to_string()), Span::call_site());
        let mod8_type = get_mod8_ident(i % 8, None);
        let uint_size = match get_uint_size(i) {
            Ok(uint_size) => uint_size,
            Err(e) => return e.to_compile_error().into(),
        };
        let uint_type_literal = format!("u{}", uint_size);
        let uint_type = Ident::new(&uint_type_literal, Span::call_site());
        let iu8 = i as u8;

        bits.push(quote! {
            pub struct #ident;
            impl Specifier for #ident {
                const BITS: u8 = #iu8;
            }
            impl Mod8Specifier for #ident {
                type Mod8Type = #mod8_type;
            }
            impl UintSpecifier for #ident {
                type Uint = #uint_type;
            }
        });
    }
    quote! {
        #(#bits)*
    }
    .into()
}

fn get_uint_size(n: usize) -> syn::Result<usize> {
    let result = match n {
        0..9 => 8,
        9..17 => 16,
        17..33 => 32,
        33..65 => 64,
        _ => return Err(syn::Error::new(Span::call_site(), "out of range of u64")),
    };

    Ok(result)
}

const PREFIX_MAP: [&str; 8] = [
    "Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven",
];

#[proc_macro]
pub fn generate_mod8_types(_: TokenStream) -> TokenStream {
    let mut mod8_types = vec![];
    for i in 0..8 {
        let ident = get_mod8_ident(i, None);
        mod8_types.push(quote! {
            pub struct #ident;
        });

        if i == 0 {
            mod8_types.push(quote! {
                impl TotalSizeIsMultipleOfEightBits for #ident {}
            });
        }
    }

    let add_trait_impls = get_add_trait_impls();

    quote! {
        #(#mod8_types)*
        #add_trait_impls
    }
    .into()
}

fn get_add_trait_impls() -> proc_macro2::TokenStream {
    let mut add_trait_impls = vec![];
    for i in 0..8 {
        for j in 0..8 {
            let ident_i = get_mod8_ident(i, None);
            let ident_j = get_mod8_ident(j, None);
            let ident_output = get_mod8_ident((i + j) % 8, None);
            add_trait_impls.push(quote! {
                impl std::ops::Add<#ident_j> for #ident_i {
                    type Output = #ident_output;
                    fn add(self, _rhs: #ident_j) -> Self::Output {
                        #ident_output
                    }
                }
            });
        }
    }

    quote! {
        #(#add_trait_impls)*
    }
}

fn get_mod8_ident(n: usize, span: Option<Span>) -> Ident {
    let ident_literal = format!("{}Mod8", PREFIX_MAP[n]);
    match span {
        Some(span) => Ident::new(&ident_literal, span),
        None => Ident::new(&ident_literal, Span::call_site()),
    }
}
