use super::*;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Ray<T: Number>(pub Point<T>, pub Vector<T>);

impl <T: Number> AlmostEq<T> for Ray<T> {
    fn almost_eq_epsilon(self, Ray(op1, op2): Ray<T>, epsilon: T) -> bool {
        let Ray(sp1, sp2) = self;
        sp1.almost_eq_epsilon(op1, epsilon) &&
        sp2.almost_eq_epsilon(op2, epsilon)
    }
}

