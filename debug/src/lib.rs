use proc_macro::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse_macro_input, parse_quote, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput,
    Expr, ExprLit, Field, GenericArgument, GenericParam, Generics, Ident, ImplGenerics, Lit, Meta,
    MetaNameValue, Path, PathArguments, Type, TypeGenerics, TypePath, WhereClause,
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
    let mut trait_tys = vec![];

    // field stage
    match &ast.data {
        Data::Struct(DataStruct { fields, .. }) => {
            for field in fields {
                let field_ident = field.ident.as_ref().unwrap();
                let field_ty = &field.ty;
                if phantom_data_generic_ty.is_none() {
                    phantom_data_generic_ty = get_generic_inner_ty(field_ty, "PhantomData");
                }
                trait_tys.push(get_trait_ty(field_ty));

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
    let where_clause_extra = vec![
        get_phantom_data_generic_ty_where_clause(phantom_data_generic_ty),
        get_trait_ty_where_clause(trait_tys),
    ];

    let merged_where_clause = merge_where_clause_extra(where_clause_extra);

    let impl_expr = impl_expr(
        struct_ident,
        &impl_generics,
        &ty_generics,
        where_clause,
        merged_where_clause,
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
        let path_last = path.segments.last()?;
        if path_last.ident.eq(outer) {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &path_last.arguments
            {
                let generic_ty = args.last()?;

                if let GenericArgument::Type(ty) = generic_ty {
                    return Some(ty);
                }
            }
        }
    }

    None
}

fn get_trait_ty(outer_ty: &Type) -> Option<&Type> {
    println!("outer_ty: {}", outer_ty.to_token_stream());
    if let Type::Path(TypePath { path, .. }) = outer_ty {
        if path.segments.len() == 2 {
            // println!("get trait ty path: {}", outer_ty.to_token_stream());
            return Some(outer_ty);
        }
    }

    if let Some(inner_ty) = get_inner_ty(outer_ty) {
        return get_trait_ty(inner_ty);
    }

    None
}

fn get_inner_ty(outer_ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath { path, .. }) = outer_ty {
        let path_last = path.segments.last()?;
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
            &path_last.arguments
        {
            let generic_ty = args.last().unwrap();
            if let GenericArgument::Type(inner_ty) = generic_ty {
                return Some(inner_ty);
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
    where_clause_extra: Option<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let where_clause = get_where_clause(where_clause, where_clause_extra);
    quote! {
        impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics
        #where_clause
    }
}

fn get_where_clause(
    where_clause: Option<&WhereClause>,
    where_clause_extra: Option<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    match (where_clause, where_clause_extra) {
        (Some(where_clause), Some(where_clause_extra)) => {
            quote! {
                #where_clause
                #where_clause_extra,
            }
        }
        (_, None) => {
            quote! {#where_clause}
        }
        (_, Some(where_clause_extra)) => {
            quote! {
                where
                #where_clause_extra
            }
        }
    }
}

fn get_phantom_data_generic_ty_where_clause(
    phantom_data_generic_ty: Option<&Type>,
) -> Option<proc_macro2::TokenStream> {
    let ty = phantom_data_generic_ty?;
    Some(quote! {
        PhantomData<#ty>: std::fmt::Debug,
    })
}

fn get_trait_ty_where_clause(trait_tys: Vec<Option<&Type>>) -> Option<proc_macro2::TokenStream> {
    let any_some = trait_tys
        .iter()
        .fold(false, |acc, trait_ty_path| acc | trait_ty_path.is_some());

    if !any_some {
        return None;
    }

    let mut result = quote! {};
    for trait_ty in trait_tys {
        if let Some(trait_ty) = trait_ty {
            result.extend(quote! {
                #trait_ty: std::fmt::Debug,
            });
        }
    }

    Some(result)
}

fn merge_where_clause_extra(
    where_clause_extra: Vec<Option<proc_macro2::TokenStream>>,
) -> Option<proc_macro2::TokenStream> {
    let mut result: Option<proc_macro2::TokenStream> = None;
    for where_clause in where_clause_extra {
        if let Some(where_clause) = where_clause {
            if let Some(ref mut result) = result {
                result.extend(where_clause);
            } else {
                result = Some(where_clause);
            }
        }
    }

    result
}
