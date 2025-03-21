use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput,
    Expr, ExprLit, Field, GenericArgument, GenericParam, Generics, Ident, ImplGenerics, Lit, Meta,
    MetaNameValue, PathArguments, Type, TypeGenerics, TypePath, WhereClause,
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
    let mut phantom_data_generic_ty = None;

    // field stage
    match &ast.data {
        Data::Struct(DataStruct { fields, .. }) => {
            for field in fields {
                let field_ident = field.ident.as_ref().unwrap();
                let field_ty = &field.ty;
                if phantom_data_generic_ty.is_none() {
                    phantom_data_generic_ty = get_generic_inner_ty(field_ty, "PhantomData");
                }

                let attr_debug_lit = get_attr_debug_lit(field);
                field_calls.extend(field_call(field_ident, attr_debug_lit));
            }
        }
        _ => unimplemented!(),
    };
    // generic bound stage
    let generics = ast.generics;
    let generics = add_trait_bounds(generics, phantom_data_generic_ty);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_expr = impl_expr(
        struct_ident,
        &impl_generics,
        &ty_generics,
        where_clause,
        phantom_data_generic_ty,
    );
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

fn get_generic_inner_ty<'a>(field_ty: &'a Type, outer: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath { path, .. }) = field_ty {
        let path_last = path.segments.last().unwrap();
        if path_last.ident.eq(outer) {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &path_last.arguments
            {
                let generic_ty = args.last().unwrap();

                if let GenericArgument::Type(ty) = generic_ty {
                    return Some(ty);
                }
            }
        }
    }

    None
}

fn add_trait_bounds(mut generics: Generics, phantom_data_generic_ty: Option<&Type>) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if let Some(ty) = phantom_data_generic_ty {
                let ty_ident: Ident = parse_quote!(#ty);
                if type_param.ident.eq(&ty_ident) {
                    continue;
                }
            }
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }

    generics
}

fn impl_expr(
    struct_ident: &Ident,
    impl_generics: &ImplGenerics,
    ty_generics: &TypeGenerics,
    where_clause: Option<&WhereClause>,
    phantom_data_generic_ty: Option<&Type>,
) -> proc_macro2::TokenStream {
    let where_clause = get_where_clause(where_clause, phantom_data_generic_ty);
    quote! {
        impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics
        #where_clause
    }
}

fn get_where_clause(
    where_clause: Option<&WhereClause>,
    phantom_data_generic_ty: Option<&Type>,
) -> proc_macro2::TokenStream {
    match (where_clause, phantom_data_generic_ty) {
        (Some(where_clause), Some(ty)) => {
            quote! {
                #where_clause
                PhantomData<#ty>: std::fmt::Debug,
            }
        }
        (_, None) => {
            quote! {#where_clause}
        }
        (_, Some(ty)) => {
            quote! {
                where
                PhantomData<#ty>: std::fmt::Debug,
            }
        }
    }
}
