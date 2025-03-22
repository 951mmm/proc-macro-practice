use std::io::Bytes;

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Field, Fields, FieldsNamed, Item,
    ItemStruct, PathSegment, Type, TypePath,
};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut ast = parse_macro_input!(input as Item);

    match resolve_bitfield(&mut ast) {
        Ok(_) => ast.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn resolve_bitfield(ast: &mut Item) -> syn::Result<()> {
    match ast {
        Item::Struct(ItemStruct { attrs, fields, .. }) => {
            resolve_attr(attrs);
            let new_field = get_field(fields)?;
            let new_fields: FieldsNamed = parse_quote! {
                {
                    #new_field,
                }
            };

            *fields = new_fields.into();
        }
        _ => return Err(syn::Error::new(ast.span(), "expected struct")),
    };

    Ok(())
}

// add '#[repr_c]' attr
fn resolve_attr(attrs: &mut Vec<Attribute>) {
    debug_assert_eq!(attrs.len(), 0);
    attrs.push(parse_quote! {#[repr(C)]});
}

fn get_field(fields: &Fields) -> syn::Result<proc_macro2::TokenStream> {
    const BYTE_LEN: u8 = 8;

    let mut size_sum = 0;
    for field in fields {
        if let Type::Path(TypePath { path, .. }) = &field.ty {
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

            size_sum += size;
        } else {
            return Err(syn::Error::new(
                field.ty.span(),
                "field type should be Path-Like",
            ));
        }
    }

    let size_sum_lit = Literal::u8_unsuffixed(size_sum / BYTE_LEN);
    Ok(quote! {
        data: [u8; #size_sum_lit]
    })
}
