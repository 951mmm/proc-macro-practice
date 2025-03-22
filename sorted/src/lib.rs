use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Item};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = args;
    let ast = parse_macro_input!(input as Item);

    match expand(args, ast) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand(args: TokenStream, ast: Item) -> syn::Result<proc_macro2::TokenStream> {
    println!("ast: {}", ast.to_token_stream());

    match ast {
        Item::Struct(_) => {
            return Err(syn::Error::new(
                ast.span(),
                "expected enum or match expression",
            ))
        }
        _ => {}
    }

    Ok(quote! {})
}
