use super::*;

pub enum LineSegmentSplit<T: Number> {
    NoBreak(LineSegment<T>),
    Bisected(LineSegment<T>, LineSegment<T>),
    Overlapping(LineSegment<T>, LineSegment<T>, LineSegment<T>),
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct LineSegment<T: Number>(pub Point<T>, pub Point<T>);

impl <T: Number> LineSegment<T> {
    pub fn length_squared(self) -> T {
        let LineSegment(Point(x1, y1), Point(x2, y2)) = self;
        (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
    }

    pub fn length(self) -> T {
        self.length_squared().sqrt()
    }

    pub fn midpoint(self) -> Point<T> {
        let two = T::one() + T::one();
        let LineSegment(p1, p2) = self;
        let mut v = p1 - p2;
        v.0 = v.0 / two;
        v.1 = v.1 / two;
        p2 + v
    }
}

impl <N: Number> Intersects<LineSegment<N>, N> for LineSegment<N> {
    fn intersects(self, other: LineSegment<N>) -> Intersections<N> {
        let LineSegment(Point(p0_x, p0_y), Point(p1_x, p1_y)) = self;
        let LineSegment(Point(p2_x, p2_y), Point(p3_x, p3_y)) = other;

        let s1_x = p1_x - p0_x;
        let s1_y = p1_y - p0_y;
        let s2_x = p3_x - p2_x;
        let s2_y = p3_y - p2_y;

        let s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y);
        let t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y);

        if s.is_nan() && t.is_nan() {
            match (other.distance(self.0).almost_eq(N::zero()),
                   other.distance(self.1).almost_eq(N::zero()),
                   self.distance(other.0).almost_eq(N::zero()),
                   self.distance(other.1).almost_eq(N::zero())) {
                // Line segments are the same
                (true, true, true, true) => Intersections::None,

                // self is contained within other
                (true, true, false, false) => Intersections::Two(self.0, self.1),
                // self has one point inside other, and shares another point with other
                (true, true, true, false) => Intersections::One(if (self.0).almost_eq(other.0) { self.1 } else { self.0 }),
                (true, true, false, true) => Intersections::One(if (self.0).almost_eq(other.1) { self.1 } else { self.0 }),

                // other is contained within self
                (false, false, true, true) => Intersections::Two(other.0, other.1),
                // other has one point inside self, and shares another point with self
                (true, false, true, true) => Intersections::One(if (other.0).almost_eq(self.0) { other.1 } else { other.0 }),
                (false, true, true, true) => Intersections::One(if (other.0).almost_eq(self.1) { other.1 } else { other.0 }),

                // they form a longer line
                (true, false, true, false) |
                (false, true, false, true) |
                (true, false, false, true) |
                (false, true, true, false) => Intersections::None,

                // They aren't even close to each other
                (false, false, false, false) => Intersections::None,

                (true, false, false, false) |
                (false, true, false, false) |
                (false, false, true, false) |
                (false, false, false, true) => panic!("these lines showed up as being colinear but they were not: {:?} {:?}", self, other),
            }
        } else if s >= N::zero() && s <= N::one() && t >= N::zero() && t <= N::one() {
            Intersections::One(Point(p0_x + (t * s1_x), p0_y + (t * s1_y)))
        } else {
            Intersections::None
        }
    }
}

impl <N: Number> Intersects<Circle<N>, N> for LineSegment<N> {
    fn intersects(self, other: Circle<N>) -> Intersections<N> {
        other.intersects(self)
    }
}

impl <T: Number> Distance<Point<T>, T> for LineSegment<T> {
    fn distance_squared(self, point: Point<T>) -> T {
        point.distance_squared(self)
    }
}

impl <T: Number> Bounded<T> for LineSegment<T> {
    fn aabb(self) -> Rect<T> {
        let LineSegment(p1, p2) = self;
        let mut res = Rect::null_at(p1);
        res.expand_to_include(p2);
        res
    }
}

impl <T: Number> AlmostEq<T> for LineSegment<T> {
    fn almost_eq_epsilon(self, LineSegment(op1, op2): LineSegment<T>, epsilon: T) -> bool {
        let LineSegment(sp1, sp2) = self;
        sp1.almost_eq_epsilon(op1, epsilon) &&
        sp2.almost_eq_epsilon(op2, epsilon) ||
        sp1.almost_eq_epsilon(op2, epsilon) &&
        sp2.almost_eq_epsilon(op1, epsilon)
    }
}

impl <T: Number> SplitBy<LineSegment<T>> for LineSegment<T> {
    type Out = LineSegmentSplit<T>;
    fn split_by(self, other: LineSegment<T>) -> LineSegmentSplit<T> {
        let LineSegment(f, l) = self;
        match self.intersects(other) {
            Intersections::One(b) =>
                LineSegmentSplit::Bisected(LineSegment(f, b), LineSegment(b, l)),
            Intersections::Two(a, b) if a != f && b != l => 
                LineSegmentSplit::Overlapping(LineSegment(f, a), LineSegment(a, b), LineSegment(b, l)),
            _ => LineSegmentSplit::NoBreak(self),
        }
    }
}
