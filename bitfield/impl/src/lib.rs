use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Fields, FieldsNamed, Item,
    ItemStruct, Type, TypePath,
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
            resolve_fields(fields)?;
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

fn resolve_fields(fields: &mut Fields) -> syn::Result<()> {
    const BYTE_LEN: u8 = 8;

    let mut size_sum = 0;
    for field in fields.iter_mut() {
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
    let new_fields: FieldsNamed = parse_quote! {
        {
            data: [u8; #size_sum_lit],
        }
    };
    *fields = new_fields.into();

    Ok(())
}
