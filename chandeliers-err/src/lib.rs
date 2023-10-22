use proc_macro2::Span;
use std::fmt::Display;

pub type Error = Vec<(String, Option<Span>)>;
pub trait IntoError {
    fn into_err(self) -> Error;
}
pub trait TrySpan {
    fn try_span(&self) -> Option<Span> {
        None
    }
}

impl TrySpan for Span {
    fn try_span(&self) -> Option<Span> {
        Some(*self)
    }
}
impl<T: TrySpan> TrySpan for &T {
    fn try_span(&self) -> Option<Span> {
        (*self).try_span()
    }
}
impl<T: TrySpan> TrySpan for Option<T> {
    fn try_span(&self) -> Option<Span> {
        self.as_ref().and_then(|t| t.try_span())
    }
}

pub struct Basic {
    pub msg: String,
    pub span: Span,
}

impl IntoError for Basic {
    fn into_err(self) -> Error {
        vec![(self.msg, Some(self.span))]
    }
}

pub struct TypeMismatch<Source, Left, Right, Msg> {
    pub source: Source,
    pub left: Left,
    pub right: Right,
    pub msg: Msg,
}

impl<Source, Left, Right, Msg> IntoError for TypeMismatch<Source, Left, Right, Msg>
where
    Source: TrySpan,
    Msg: Display,
    Left: Display + TrySpan,
    Right: Display + TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!(
                "Type mismatch between the left and right sides: {}",
                self.msg
            ),
            self.source.try_span(),
        ));
        v.push((
            format!("This element has type {}", self.left),
            self.left.try_span(),
        ));
        v.push((
            format!("While this element has type {}", self.right),
            self.right.try_span(),
        ));
        v
    }
}

pub struct VarNotFound<Var, Suggest1, Suggest2> {
    pub var: Var,
    pub suggest1: Suggest1,
    pub suggest2: Suggest2,
}

impl<Var, Suggest1, S1, Suggest2, S2> IntoError for VarNotFound<Var, Suggest1, Suggest2>
where
    Var: Display + TrySpan,
    Suggest1: IntoIterator<Item = S1>,
    Suggest2: IntoIterator<Item = S2>,
    S1: Display,
    S2: Display,
{
    fn into_err(self) -> Vec<(String, Option<Span>)> {
        let mut suggest1 = self
            .suggest1
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>();
        let mut suggest2 = self
            .suggest2
            .into_iter()
            .map(|v| format!("{v}"))
            .collect::<Vec<_>>();
        suggest1.sort();
        suggest2.sort();
        let suggest1 = if suggest1.is_empty() {
            String::from("(none declared)")
        } else {
            suggest1.join(", ")
        };
        let suggest2 = if suggest2.is_empty() {
            String::from("(none declared)")
        } else {
            suggest2.join(", ")
        };

        let mut v = vec![];
        v.push((
            format!("Variable {} not found in the context.", self.var),
            self.var.try_span(),
        ));
        v.push((
            format!("Perhaps you meant one of the local variables: {}", suggest1),
            None,
        ));
        v.push((
            format!("or one of the global variables: {}", suggest2),
            None,
        ));
        v
    }
}

pub struct NotConst<Item, Site> {
    pub what: Item,
    pub site: Site,
}

impl<Item, Site> IntoError for NotConst<Item, Site>
where
    Item: Display,
    Site: TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!("{} not valid in const contexts", self.what),
            self.site.try_span(),
        ));
        v.push((format!("You must put this definition inside a node"), None));
        v
    }
}

pub struct BinopMismatch<Oper, Site, Expect, Left, Right> {
    pub oper: Oper,
    pub site: Site,
    pub expect: Expect,
    pub left: Left,
    pub right: Right,
}

impl<Oper, Site, Expect, Left, Right> IntoError for BinopMismatch<Oper, Site, Expect, Left, Right>
where
    Oper: Display,
    Site: TrySpan,
    Expect: Display,
    Left: Display + TrySpan,
    Right: Display + TrySpan,
{
    fn into_err(self) -> Vec<(String, Option<Span>)> {
        let mut v = vec![];
        v.push((
            format!(
                "Binary operator {} expects arguments of {}",
                self.oper, self.expect
            ),
            self.site.try_span(),
        ));
        v.push((
            format!("The left-hand-side is found to be of type {}", self.left),
            self.left.try_span(),
        ));
        v.push((
            format!("The right-hand-side is found to be of type {}", self.right),
            self.right.try_span(),
        ));
        v
    }
}

pub struct UnopMismatch<Oper, Expect, Site, Inner> {
    pub oper: Oper,
    pub expect: Expect,
    pub site: Site,
    pub inner: Inner,
}

impl<Oper, Expect, Site, Inner> IntoError for UnopMismatch<Oper, Expect, Site, Inner>
where
    Oper: Display,
    Expect: Display,
    Site: TrySpan,
    Inner: Display + TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!(
                "Unary operator {} expects an argument of {}",
                self.oper, self.expect
            ),
            self.site.try_span(),
        ));
        v.push((
            format!("The inner value is found to be of type {}", self.inner),
            self.inner.try_span(),
        ));
        v
    }
}

pub struct BoolRequired<Type, Site, Inner> {
    pub actual: Type,
    pub site: Site,
    pub inner: Inner,
}

impl<Type, Site, Inner> IntoError for BoolRequired<Type, Site, Inner>
where
    Type: Display,
    Site: TrySpan,
    Inner: Display + TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!("{} should be of type bool", self.actual),
            self.site.try_span(),
        ));
        v.push((
            format!("The argument is found to be of type {}", self.inner),
            self.inner.try_span(),
        ));
        v
    }
}

pub struct Cycle<Items> {
    pub items: Items,
}

impl<Items, Item> IntoError for Cycle<Items>
where
    Items: IntoIterator<Item = (Item, Option<Span>)>,
    Item: Display + TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        for (i, (it, sp)) in self.items.into_iter().enumerate() {
            v.push((
                if i == 0 {
                    format!("{it} was found to be part of a dependency cycle")
                } else {
                    format!("The cycle also goes through {it}")
                },
                sp,
            ));
        }
        v
    }
}

pub struct GraphUnitDeclTwice<Unit, NewSite, Prior, PriorSite> {
    pub unit: Unit,
    pub new_site: NewSite,
    pub prior: Prior,
    pub prior_site: PriorSite,
}

impl<Unit, NewSite, Prior, PriorSite> IntoError
    for GraphUnitDeclTwice<Unit, NewSite, Prior, PriorSite>
where
    Unit: Display,
    Prior: Display,
    NewSite: TrySpan,
    PriorSite: TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!(
                "Attempt to redefine {}, when {} already defines it",
                self.unit, self.prior
            ),
            self.new_site.try_span(),
        ));
        v.push((
            String::from("Already defined here"),
            self.prior_site.try_span(),
        ));
        v
    }
}

pub struct GraphUnitUndeclared<Unit> {
    pub unit: Unit,
}

impl<Unit> IntoError for GraphUnitUndeclared<Unit>
where
    Unit: Display + TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!("No definition provided for {} which is required", self.unit),
            self.unit.try_span(),
        ));
        v
    }
}

pub struct GraphUnitDependsOnItself<Unit, Site1, Site2> {
    pub unit: Unit,
    pub def_site: Site1,
    pub usage: Site2,
}

impl<Unit, Site1, Site2> IntoError for GraphUnitDependsOnItself<Unit, Site1, Site2>
where
    Unit: Display,
    Site1: TrySpan,
    Site2: TrySpan,
{
    fn into_err(self) -> Error {
        let mut v = vec![];
        v.push((
            format!("{} depends on itself", self.unit),
            self.def_site.try_span(),
        ));
        v.push((
            String::from("used here within its own definition"),
            self.usage.try_span(),
        ));
        v
    }
}
