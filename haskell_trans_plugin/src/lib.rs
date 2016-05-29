#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate rustc;
extern crate rustc_plugin;
use syntax::codemap::Span;
use syntax::parse::token::{self, intern};
use syntax::ast::{TokenTree, MetaItem, Mutability, Item, ItemKind};
use syntax::ext::build::AstBuilder;  // trait for expr_uint
use syntax::ext::base::*;
use rustc_plugin::Registry;
use std::os::raw::c_void;
pub trait FromHaskellRepr {
    fn from_haskell_repr(stablePtr : *mut std::os::raw::c_void) -> Self;
}
fn expand_from_haskell_repr(cx: &mut ExtCtxt, span: Span, mitem: &MetaItem, item: &Annotatable,
                            push: &mut FnMut(Annotatable)) {
    let node = match *item {
        Annotatable::Item(ref item) => match item.clone().unwrap() {
            Item { node, .. } => node
        },
        _ => panic!("trait and impl unsupported")
    };

    let trait_def = TraitDef {
        is_unsafe: false,
        span: span,
        attributes: Vec::new(),
        path: ty::Path::new(vec!("haskell_trans_plugin", "FromHaskellRepr")),
        additional_bounds: Vec::new(),
        generics: ty::LifetimeBounds::empty(),
        methods: vec![
            MethodDef {
                name: "from_haskell_repr",
                generics: ty::LifetimeBounds::empty(),
                explicit_self: None,
                args: vec!(ty::Ptr(box ty::Literal(ty::Path::new(vec!("std", "os", "raw", "c_void"))),
                                   ty::Raw(Mutability::Mutable))),
                ret_ty: ty::Ty::Self_,
                attributes: Vec::new(),
                is_unsafe: false,
                combine_substructure: combine_substructure(Box::new(haskell_repr_substructure)),
                unify_fieldless_variants: true,
            }
        ],
        associated_types: Vec::new(),
    };
    trait_def.expand(cx, mitem, item, push)
    println!("{:?}", node);
//    unimplemented!()
}
fn haskell_repr_substructure(cx: &mut ExtCtxt, trait_span: Span, substr: &Substructure) -> P<Expr> {
    let state_expr = if substr.nonself_args.len() == 1 {
        &substr.nonself_args[0]
    } else {
        cx.span_bug(trait_span, "incorrect number of arguments in `from_haskell_repr`")
    };
    let trace_ident = substr.method_ident;
    let call_trace = |span, thing_expr| {
        let expr = cx.expr_method_call(span, thing_expr, trace_ident, vec!(state_expr.clone()));
        cx.stmt_expr(expr)
    };
    let mut stmts = Vec::new();

    let fields = match *substr.fields {
        Struct(_, ref fs) | EnumMatching(_, _, ref fs) => fs,
        _ => cx.span_bug(trait_span, "impossible substructure in `jstraceable`")
    };

    for &FieldInfo { ref self_, span, .. } in fields {
        stmts.push(call_trace(span, self_.clone()));
    }

    cx.expr_block(cx.block(trait_span, stmts, None))
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(intern("derive_FromHaskellRepr"), MultiDecorator(Box::new(expand_from_haskell_repr)));
}
