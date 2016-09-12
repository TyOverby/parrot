mod rect;
mod point;
mod vector;
mod line_segment;
mod circle;
mod ray;
mod poly;
pub use self::rect::*;
pub use self::point::*;
pub use self::vector::*;
pub use self::line_segment::*;
pub use self::circle::*;
pub use self::ray::*;
pub use self::poly::*;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Intersections<N: Number> {
    None,
    One(Point<N>),
    Two(Point<N>, Point<N>),
    Many(Vec<Point<N>>)
}

pub trait Number: ::num::traits::Float + ::num::traits::FromPrimitive + ::std::cmp::PartialOrd + Copy + ::std::fmt::Debug {}

pub trait Distance<T, N: Number> {
    fn distance_squared(self, other: T) -> N;
    fn distance(self, other: T) -> N where Self: Sized {
        self.distance_squared(other).sqrt()
    }
}

pub trait Translate<N: Number> {
    fn translate_v(self, v: Vector<N>) -> Self where Self: Sized {
        self.translate(v.0, v.1)
    }
    fn translate(self, x: N, y: N) -> Self;
}

pub trait Intersects<T, N: Number> {
    fn intersects(self, other: T) -> Intersections<N>;
}

pub trait SplitBy<T> {
    type Out;
    fn split_by(self, other: T) -> Self::Out;
}

pub trait Bounded<T: Number> {
    fn aabb(self) -> Rect<T>;
}

pub trait Contains<T> {
    fn contains(self, T) -> bool;
}

pub trait AlmostEq<N: Number> {
    fn almost_eq_epsilon(self, Self, N) -> bool;
    fn almost_eq(self, other: Self) -> bool where Self: Sized {
        self.almost_eq_epsilon(other, N::min_positive_value() * N::from_u64(100).unwrap())
    }
}


impl <T: ::num::traits::Float + ::num::traits::FromPrimitive + ::std::cmp::PartialOrd + Copy + ::std::fmt::Debug> Number for T {}

pub fn clamp<T: Number>(low: T, value: T, high: T) -> T{
    T::min(high, T::max(low, value))
}

impl <T: Number> AlmostEq<T> for T {
    fn almost_eq_epsilon(self, other: T, epsilon: T) -> bool {
        (self - other).abs() < epsilon 
    }
}

impl <T: Number> AlmostEq<T> for Intersections<T> {
    fn almost_eq_epsilon(self, other: Intersections<T>, epsilon: T) -> bool {
        match (self, other) {
            (Intersections::None, Intersections::None) => true,
            (Intersections::One(a), Intersections::One(b)) => a.almost_eq_epsilon(b, epsilon),
            (Intersections::Two(a, b), Intersections::Two(c, d)) => a.almost_eq_epsilon(c, epsilon) && b.almost_eq_epsilon(d, epsilon),
            (Intersections::Many(a), Intersections::Many(b)) => a.into_iter().zip(b.into_iter()).all(|(a, b)| a.almost_eq_epsilon(b, epsilon)),
            _ => false
        }
    }
}

impl <T: Number> ::std::fmt::Debug for Intersections<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &Intersections::None => write!(f, "Intersections::None"),
            &Intersections::One(Point(x, y)) => write!(f, "Intersections::One(Point({:.20?}, {:.20?}))", x, y),
            &Intersections::Two(Point(x1, y1), Point(x2, y2)) => write!(f, "Intersections::Two(Point({:.20?}, {:.20?}), Point({:.20?}, {:.20?}))", x1, y1, x2, y2),
            &Intersections::Many(ref v) => {
                try!(write!(f, "Intersections::Many(vec!["));
                for &Point(x, y) in v {
                    try!(write!(f, "Point({:.20?}, {:.20?}),", x, y));
                }
                write!(f, "])")
            }
        }
    }
}
