use proc_macro::TokenStream;
use syn::{
    self,
    parse_macro_input,
};
use quote::{
    format_ident,
    quote,
};
use litrs::Literal;
use convert_case::{
    Case,
    Casing,
};
use thiserror::Error;

#[derive(Error, Debug)]
enum QLispMacroError {
    #[error("invalid alias type for #[builtin]")]
    InvalidBuiltinAliasType,
    #[error("invalid type for #[builtin] argument 'name': must be a string literal")]
    InvalidBuiltinName,
    #[error("missing #[builtin] argument 'name'")]
    MissingBuiltinName,
}
type QLispMacroResult<T> = Result<T, QLispMacroError>;

/// Create a built-in function with help text set by the function's docstring.
/// Lines in the docstring beginning with \`\`\` are discarded.
/// Optionally set the built-in's alias.
///
/// ## Example
/// ```rust,ignore
/// use qlisp_macros::builtin;
///
/// /// This defines a built-in's help text when `(help)` is called in the REPL.
/// #[builtin(name = "foo", alias = "f")]
/// pub fn foo(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
///     // function implemented here
/// }
/// ```
/// The above code is then expanded to
/// ```rust,ignore
/// /// This defines a built-in's help text when `(help)` is called in the REPL.
/// pub fn foo(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
///     // function implemented here
/// }
///
/// pub const Foo: QBuiltin = QBuiltin {
///     f: foo,
///     name: "foo",
///     help_text: "This defines a built-in's help text when `(help)` is called in the REPL",
///     alias: Some("f"),
/// }
/// ```
/// Use of this macro relies on a definition of the `QBuiltin` struct in scope:
/// ```rust,ignore
/// pub struct QBuiltin {
///     f: fn(&mut QEnv, &[QExp]) -> QResult<QExp>,
///     name: &'static str,
///     help_text: &'static str,
///     alias: Option<&'static str>,
/// }
/// ```
#[proc_macro_attribute]
pub fn builtin(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as syn::AttributeArgs);
    let fn_ast: syn::ItemFn = syn::parse(item)
        .expect("invalid function definition");
    let fn_name: String = isolate_fn_name(&fn_ast);
    let fn_ident = format_ident!("{}", fn_name);
    let struct_ident = format_ident!("{}", fn_name.to_case(Case::UpperCamel));
    let builtin_name: String = get_name(&args)
        .expect(
            "missing or invalid value for 'name': must be a string literal"
        );
    let docstring: String = isolate_docstring(&fn_ast);
    let alias: Option<String> = get_alias(&args)
        .expect("argument for 'alias' ust be a string literal");
    let item_tokens = quote! {
        #fn_ast
    };
    let alias_value = match alias {
        Some(a) => quote! { Some(#a) },
        None => quote! { None },
    };
    let struct_tokens = quote! {
        pub const #struct_ident: QBuiltin = QBuiltin {
            f: #fn_ident,
            name: #builtin_name,
            help_text: #docstring,
            alias: #alias_value,
        };
    };
    let tokens = quote! {
        #item_tokens
        #struct_tokens
    };
    return tokens.into();
}

fn get_name(args: &syn::AttributeArgs) -> QLispMacroResult<String> {
    for arg in args.clone().into_iter() {
        if let syn::NestedMeta::Meta(meta) = arg {
            if let syn::Meta::NameValue(namevalue) = meta {
                if let Some(ident) = namevalue.path.get_ident() {
                    if ident == "name" {
                        if let syn::Lit::Str(s) = namevalue.lit {
                            return Ok(s.value());
                        } else {
                            return Err(QLispMacroError::InvalidBuiltinName);
                        }
                    }
                }
            }
        }
    }
    return Err(QLispMacroError::MissingBuiltinName);
}

fn get_alias(args: &syn::AttributeArgs) -> QLispMacroResult<Option<String>> {
    for arg in args.clone().into_iter() {
        if let syn::NestedMeta::Meta(meta) = arg {
            if let syn::Meta::NameValue(namevalue) = meta {
                if let Some(ident) = namevalue.path.get_ident() {
                    if ident == "alias" {
                        if let syn::Lit::Str(s) = namevalue.lit {
                            return Ok(Some(s.value()));
                        } else {
                            return Err(
                                QLispMacroError::InvalidBuiltinAliasType
                            );
                        }
                    }
                }
            }
        }
    }
    return Ok(None);
}

fn isolate_fn_name(ast: &syn::ItemFn) -> String {
    return ast.sig.ident.to_string();
}

fn isolate_docstring(ast: &syn::ItemFn) -> String {
    let mut doclines: Vec<String> = Vec::new();
    for attr in ast.attrs.iter() {
        match attr.path.get_ident() {
            None => { continue; },
            Some(ident) => {
                if ident == "doc" {
                    attr.tokens.clone().into_iter()
                        .for_each(|tt| match Literal::try_from(tt) {
                            Ok(Literal::String(s)) => {
                                let line = s.value().trim();
                                if !line.starts_with("```") {
                                    doclines.push(line.to_string());
                                }
                            },
                            _ => { },
                        });
                }
            },
        }
    }
    return doclines.join("\n");
}

