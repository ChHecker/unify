pub type Result<T> = core::result::Result<T, String>;

/// A utility trait in order to turn Option<Result<...>>
/// into Result<Option<...>> and the other way around.
pub trait Transpose<T>
where
    Self: Sized,
{
    type Outer<I>;
    type Inner<S>;

    fn transpose(self) -> Self::Inner<Self::Outer<T>>;
}

impl<T> Transpose<T> for Result<Option<T>> {
    type Outer<I> = Result<I>;
    type Inner<S> = Option<S>;

    fn transpose(self) -> Option<Result<T>> {
        match self {
            Ok(value) => Some(Ok(value?)),
            Err(err) => Some(Err(err)),
        }
    }
}

impl<T> Transpose<T> for Option<Result<T>> {
    type Outer<I> = Option<I>;
    type Inner<S> = Result<S>;

    fn transpose(self) -> Result<Option<T>> {
        match self {
            Some(value) => Ok(Some(value?)),
            None => Ok(None),
        }
    }
}

/// A utility trait in order to turn Option<Result<&...>>
/// into Result<Option<&...>> and the other way around.
pub trait TransposeRef<'a, T>
where
    Self: Sized,
{
    type Outer<I>;
    type Inner<S>;

    fn transpose(self) -> Self::Inner<Self::Outer<&'a T>>;
}

impl<'a, T> TransposeRef<'a, T> for Result<&'a Option<T>> {
    type Outer<I> = Result<I>;
    type Inner<S> = Option<S>;

    fn transpose(self) -> Option<Result<&'a T>> {
        match self {
            Ok(value) => Some(Ok(value.as_ref()?)),
            Err(err) => Some(Err(err)),
        }
    }
}

impl<'a, T> TransposeRef<'a, T> for Option<&'a Result<T>> {
    type Outer<I> = Option<I>;
    type Inner<S> = Result<S>;

    fn transpose(self) -> Result<Option<&'a T>> {
        match self {
            Some(value) => Ok(Some(match value.as_ref() {
                Ok(value) => value,
                Err(error) => return Err(error.clone()),
            })),
            None => Ok(None),
        }
    }
}
