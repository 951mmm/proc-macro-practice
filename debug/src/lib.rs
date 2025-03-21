use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DataStruct, DeriveInput, Expr, ExprLit,
    Field, GenericParam, Generics, Ident, ImplGenerics, Lit, Meta, MetaNameValue, Type,
    TypeGenerics, TypeParam, WhereClause,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    match expand(ast) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand(ast: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = &ast.ident;
    let mut field_calls = quote! {};

    // field stage
    match &ast.data {
        Data::Struct(DataStruct { fields, .. }) => {
            for field in fields {
                let field_ident = field.ident.as_ref().unwrap();
                let attr_debug_lit = get_attr_debug_lit(field);
                field_calls.extend(field_call(field_ident, attr_debug_lit));
            }
        }
        _ => unimplemented!(),
    };

    // generic bound stage
    let generics = ast.generics;
    let generics = add_trait_bounds(generics);
    let (impl_generics, ty_generics, where_clause) = &generics.split_for_impl();
    let impl_expr = impl_expr(struct_ident, &impl_generics, &ty_generics, &where_clause);

    let result = quote! {
        #impl_expr {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_ident))
                    #field_calls
                    .finish()
            }
        }
    };

    Ok(result)
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn impl_expr(
    struct_ident: &Ident,
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> proc_macro2::TokenStream {
    match where_clause {
        Some(where_clause) => quote! {
            impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics
            #where_clause
        },
        None => quote! {
            impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics
        },
    }
}

fn field_call(field_ident: &Ident, attr_debug_lit: Option<&Lit>) -> proc_macro2::TokenStream {
    match attr_debug_lit {
        Some(lit) => quote! {
            .field(stringify!(#field_ident), &format_args!(#lit, self.#field_ident))
        },
        None => quote! {
            .field(stringify!(#field_ident), &self.#field_ident)
        },
    }
}

fn get_attr_debug_lit(field: &Field) -> Option<&Lit> {
    for attr in &field.attrs {
        if attr.path().is_ident("debug") {
            if let Meta::NameValue(MetaNameValue { value, .. }) = &attr.meta {
                if let Expr::Lit(ExprLit { lit, .. }) = value {
                    return Some(lit);
                }
            }
        }
    }

    None
}
