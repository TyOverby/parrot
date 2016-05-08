use super::*;
use std::ops::*;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Point<T: Number>(pub T, pub T);

impl <T: Number> Point<T> {
    pub fn distance_to(self, other: Point<T>) -> T {
        (self - other).magnitude()
    }
}

impl <T: Number> Distance<Point<T>, T> for Point<T> {
    fn distance_squared(self, other: Point<T>) -> T {
        let dx = self.0 - other.0;
        let dy = self.1 - other.1;
        dx * dx + dy * dy
    }
}

impl <T:Number> Distance<LineSegment<T>, T> for Point<T> {
    fn distance_squared(self, LineSegment(v, w): LineSegment<T>) -> T {
        use ::num::{Zero, One};
        let l2 = v.distance_squared(w);
        if l2 == Zero::zero() { // TODO: epsilon
            return self.distance_squared(v);
        }
        let t = ((self.0 - v.0) * (w.0 - v.0) + (self.1 - v.1) * (w.1 - v.1)) / l2;
        if t < Zero::zero() {
            self.distance_squared(v)
        } else if t > One::one() {
            self.distance_squared(w)
        } else {
            self.distance_squared(Point(
                v.0 + t * (w.0 - v.0),
                v.1 + t * (w.1 - v.1)
            ))
        }
    }
}

impl <'a, T: Number> Distance<&'a Poly<T>, T> for Point<T> {
    fn distance_squared(self, polygon: &Poly<T>) -> T {
        let mut min_dist = T::infinity();
        for line in polygon.lines() {
            min_dist = min_dist.min(self.distance_squared(line));
        }
        min_dist
    }
}

impl <T: Number> Bounded<T> for Point<T> {
    fn aabb(self) -> Rect<T> {
        Rect::null_at(self)
    }
}

impl <T: Number> Sub for Point<T> {
    type Output = Vector<T>;
    fn sub(self, Point(ox, oy): Point<T>) -> Vector<T> {
        let Point(sx, sy) = self;
        Vector(sx - ox, sy - oy)
    }
}

impl <T: Number> Add<Vector<T>> for Point<T> {
    type Output = Point<T>;
    fn add(self, Vector(ox, oy): Vector<T>) -> Point<T> {
        let Point(sx, sy) = self;
        Point(sx + ox, sy + oy)
    }
}

impl <T: Number> Sub<Vector<T>> for Point<T> {
    type Output = Point<T>;
    fn sub(self, Vector(ox, oy): Vector<T>) -> Point<T> {
        let Point(sx, sy) = self;
        Point(sx - ox, sy - oy)
    }
}


impl <T: Number> AlmostEq<T> for Point<T> {
    fn almost_eq_epsilon(self, Point(ox, oy): Point<T>, epsilon: T) -> bool {
        let Point(sx, sy) = self;
        sx.almost_eq_epsilon(ox, epsilon) &&
        sy.almost_eq_epsilon(oy, epsilon)
    }
}

impl <N: Number> Translate<N> for Point<N> {
    fn translate(self, x: N, y: N) -> Point<N> {
        Point(self.0 + x, self.1 + y)
    }
}

