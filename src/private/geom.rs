use std::ops::{Add, Sub};

pub trait Number: ::num::traits::Float + ::num::traits::FromPrimitive + ::std::cmp::PartialOrd + Copy + ::std::fmt::Debug {}

pub trait Distance<T, N: Number> {
    fn distance_squared(self, other: T) -> N;
    fn distance(self, other: T) -> N where Self: Sized {
        self.distance_squared(other).sqrt()
    }
}

pub trait Intersects<T, N: Number> {
    fn intersects(self, other: T) -> Intersections<N>;
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

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Intersections<N: Number> {
    None,
    One(Point<N>),
    Two(Point<N>, Point<N>),
    Many(Vec<Point<N>>)
}

pub struct LinesFromPolyIterator<'a, T: Number + 'a> {
    inner: ::std::slice::Windows<'a, Point<T>>,
    first_and_last: Option<(Point<T>, Point<T>)>,
}

impl <T: ::num::traits::Float + ::num::traits::FromPrimitive + ::std::cmp::PartialOrd + Copy + ::std::fmt::Debug> Number for T {}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Point<T: Number>(pub T, pub T);
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Vector<T: Number>(pub T, pub T);
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct LineSegment<T: Number>(pub Point<T>, pub Point<T>);
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Circle<T: Number>(pub Point<T>, pub T);
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Ray<T: Number>(pub Point<T>, pub Vector<T>);
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Rect<T: Number>(pub Point<T>, pub Point<T>);
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Poly<T: Number> {
    points: Vec<Point<T>>,
    aabb: Rect<T>,
}

impl <T: Number> Vector<T> {
    pub fn cross(self, Vector(ox, oy): Vector<T>) -> T {
        self.0 * oy - self.1 * ox
    }

    pub fn dot(self, Vector(ox, oy): Vector<T>) -> T {
        self.0 * ox + self.1 * oy
    }
}

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

impl <T: Number> Point<T> {
    pub fn distance_to(self, other: Point<T>) -> T {
        (self - other).magnitude()
    }
}

impl <T: Number> Rect<T> {
    pub fn null_at(p: Point<T>) -> Rect<T> {
        Rect(p, p)
    }

    pub fn width(self) -> T {
        let Rect(Point(left, _), Point(right, _)) = self;
        right - left
    }

    pub fn height(self) -> T {
        let Rect(Point(_, top), Point(_, bottom)) = self;
        top - bottom
    }

    pub fn area(self) -> T {
        self.width() * self.height()
    }

    pub fn expand_to_include(&mut self, Point(px, py): Point<T>) {
        let Rect(top_left, bottom_right) = *self;
        if px < top_left.0 {
            (self.0).0 = px;
        }
        if py < top_left.1 {
            (self.0).1 = py;
        }

        if px > bottom_right.0 {
            (self.1).0 = px;
        }
        if py > bottom_right.1 {
            (self.1).1 = py;
        }
    }
}


impl <T: Number> Vector<T> {
    pub fn magnitude_2(self) -> T {
        let Vector(dx, dy) = self;
        dx * dx + dy * dy
    }

    pub fn magnitude(self) -> T {
        self.magnitude_2().sqrt()
    }
}

impl <T: Number> Poly<T> {
    pub fn new(points: Vec<Point<T>>) -> Poly<T> {
        assert!(points.len() >= 3);
        let mut aabb = Rect::null_at(points[0]);
        for point in points.iter().cloned() {
            aabb.expand_to_include(point);
        }
        Poly {
            points: points,
            aabb: aabb,
        }
    }

    pub fn dedupe_consecutive_points(&mut self) {
        let mut prev: Option<Point<T>> = None;
        self.points.retain(|&p| {
            match (prev, p) {
                (None, p) => {
                    prev = Some(p);
                    true
                }
                (Some(prev_pt), p) => {
                    if prev_pt.almost_eq(p) {
                        false
                    } else {
                        prev = Some(p);
                        true
                    }
                }
            }
        })
    }

    pub fn add_point(&mut self, point: Point<T>) {
        self.points.push(point);
    }

    pub fn points(&self) -> &[Point<T>] {
        &self.points
    }

    pub fn lines(&self) -> LinesFromPolyIterator<T> {
        let points_len = self.points.len();
        let first_and_last = if points_len != 0 {
            Some((self.points[0], self.points[points_len - 1]))
        } else {
            None
        };

        LinesFromPolyIterator {
            inner: self.points.windows(2),
            first_and_last: first_and_last,
        }
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

        if s >= N::zero() && s <= N::one() && t >= N::zero() && t <= N::one() {
            Intersections::One(Point(p0_x + (t * s1_x), p0_y + (t * s1_y)))
        } else {
            Intersections::None
        }
    }
}

impl <N: Number> Intersects<LineSegment<N>, N> for Circle<N> {
    fn intersects(self, other: LineSegment<N>) -> Intersections<N> {
        let Circle(center, radius) = self;
        let LineSegment(point_a, point_b) = other;

        let ba_x = point_b.0 - point_a.0;
        let ba_y = point_b.1 - point_a.1;
        let ca_x = center.0 - point_a.0;
        let ca_y = center.1 - point_a.1;

        let a = ba_x * ba_x + ba_y * ba_y;
        let bby2 = ba_x * ca_x + ba_y * ca_y;
        let c = ca_x * ca_x + ca_y * ca_y - radius * radius;

        let pby2 = bby2 / a;
        let q = c / a;

        let disc = pby2 * pby2 - q;
        if disc < N::zero() {
            return Intersections::None
        }

        let tmp_sqrt = disc.sqrt();
        let ab_scaling_factor_1 = -pby2 + tmp_sqrt;
        let ab_scaling_factor_2 = -pby2 - tmp_sqrt;

        let p1 = Point(point_a.0 - ba_x * ab_scaling_factor_1, point_a.1 - ba_y * ab_scaling_factor_1);
        let p2 = Point(point_a.0 - ba_x * ab_scaling_factor_2, point_a.1 - ba_y * ab_scaling_factor_2);
        let disc_zero = disc == N::zero();

        let line_midpoint = other.midpoint();
        let half_length = other.length() / (N::one() + N::one());
        let p1_range = p1.distance_to(line_midpoint) <= half_length;
        let p2_range = p2.distance_to(line_midpoint) <= half_length;

        match (disc_zero, p1_range, p2_range) {
            (true, false, _) => {
                println!("a");
                Intersections::None
            }
            (true, true, _) => {
                println!("b");
                Intersections::One(p1)
            }
            (false, true, false) => {
                println!("c");
                Intersections::One(p1)
            }
            (false, false, true) => {
                println!("d");
                println!("{:?} >= {:?}", p1.distance_to(line_midpoint), half_length);
                Intersections::One(p2)
            }
            (false, true, true) => {
                println!("e");
                Intersections::Two(p1, p2)
            }
            (false, false, false) => {
                println!("f");
                Intersections::None
            }
        }
    }
}

impl <N: Number> Intersects<Circle<N>, N> for LineSegment<N> {
    fn intersects(self, other: Circle<N>) -> Intersections<N> {
        other.intersects(self)
    }
}

impl <'a, T: Number> Contains<Point<T>> for Rect<T> {
    fn contains(self, Point(px, py): Point<T>) -> bool {
        let Rect(Point(tlx, tly), Point(brx, bry)) = self;
        px >= tlx && px < brx && py >= tly && py < bry

    }
}

impl <T: Number> Contains<LineSegment<T>> for Rect<T> {
    fn contains(self, LineSegment(pa, pb): LineSegment<T>) -> bool {
        self.contains(pa) && self.contains(pb)
    }
}

impl <T: Number> Contains<Point<T>> for Circle<T> {
    fn contains(self, point: Point<T>) -> bool {
        let Circle(center, r) = self;
        (point - center).magnitude_2() <= r * r
    }
}

impl <T: Number> Contains<LineSegment<T>> for Circle<T> {
    fn contains(self, LineSegment(pa, pb): LineSegment<T>) -> bool {
        self.contains(pa) && self.contains(pb)
    }
}

impl <'a, T: Number> Contains<Point<T>> for &'a Poly<T> {
    fn contains(self, p: Point<T>) -> bool {
        if !self.aabb.contains(p) { return false; }

        let Point(testx, testy) = p;
        let mut inside = false;
        for LineSegment(Point(vertx_i, verty_i), Point(vertx_j, verty_j)) in self.lines() {
            if  ((verty_i > testy) != (verty_j > testy)) && (testx < (vertx_j - vertx_i) * (testy - verty_i) / (verty_j-verty_i) + vertx_i) {
                   inside = !inside;
            }
        }
        inside
    }
}

impl <T: Number> Contains<Rect<T>> for Rect<T> {
    fn contains(self, Rect(p1, p2): Rect<T>) -> bool {
        self.contains(p1) && self.contains(p2)
    }
}

impl <T: Number> Contains<Rect<T>> for Circle<T> {
    fn contains(self, Rect(Point(left, top), Point(right, bottom)): Rect<T>) -> bool {
        let Circle(Point(cx, cy), radius) = self;
        let dx = (cx - left).max(right - cx);
        let dy = (cy - top).max(bottom - cy);
        return radius * radius >= dx * dx + dy * dy
    }
}

impl <T: Number> Contains<Circle<T>> for Circle<T> {
    fn contains(self, Circle(Point(ox, oy), or): Circle<T>) -> bool {
        let Circle(Point(sx, sy), sr) = self;

        let radius_diff = sr - or;
		if (radius_diff < T::zero()) { return false; }

		let dx = sx - ox;
		let dy = sy - oy;
		let dst = dx * dx + dy * dy;
		let radius_sum = sr + or;
		return (!(radius_diff * radius_diff < dst) && (dst < radius_sum * radius_sum));
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
        if l2 == Zero::zero() { //  TODO: epsilon
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

impl <T: Number> Distance<Point<T>, T> for LineSegment<T> {
    fn distance_squared(self, point: Point<T>) -> T {
        point.distance_squared(self)
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

impl <'a, T: Number> Iterator for LinesFromPolyIterator<'a, T> {
    type Item = LineSegment<T>;
    fn next(&mut self) -> Option<LineSegment<T>> {
        if let Some(next) = self.inner.next() {
            Some(LineSegment(next[0], next[1]))
        } else {
            self.first_and_last.take().map(|(a, b)| LineSegment(a, b))
        }
    }
}

impl <T: Number> Bounded<T> for Rect<T> {
    fn aabb(self) -> Rect<T> {
        self
    }
}

impl <T: Number> Bounded<T> for Point<T> {
    fn aabb(self) -> Rect<T> {
        Rect::null_at(self)
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

impl <'a, T: Number> Bounded<T> for &'a Poly<T> {
    fn aabb(self) -> Rect<T> {
        self.aabb
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

impl <T: Number> AlmostEq<T> for T {
    fn almost_eq_epsilon(self, other: T, epsilon: T) -> bool {
        (self - other).abs() < epsilon 
    }
}

impl <T: Number> AlmostEq<T> for Point<T> {
    fn almost_eq_epsilon(self, Point(ox, oy): Point<T>, epsilon: T) -> bool {
        let Point(sx, sy) = self;
        sx.almost_eq_epsilon(ox, epsilon) &&
        sy.almost_eq_epsilon(oy, epsilon)
    }
}

impl <T: Number> AlmostEq<T> for Vector<T> {
    fn almost_eq_epsilon(self, Vector(ox, oy): Vector<T>, epsilon: T) -> bool {
        let Vector(sx, sy) = self;
        sx.almost_eq_epsilon(ox, epsilon) &&
        sy.almost_eq_epsilon(oy, epsilon)
    }
}

impl <T: Number> AlmostEq<T> for Rect<T> {
    fn almost_eq_epsilon(self, Rect(op1, op2): Rect<T>, epsilon: T) -> bool {
        let Rect(sp1, sp2) = self;
        sp1.almost_eq_epsilon(op1, epsilon) &&
        sp2.almost_eq_epsilon(op2, epsilon)
    }
}

impl <T: Number> AlmostEq<T> for LineSegment<T> {
    fn almost_eq_epsilon(self, LineSegment(op1, op2): LineSegment<T>, epsilon: T) -> bool {
        let LineSegment(sp1, sp2) = self;
        sp1.almost_eq_epsilon(op1, epsilon) &&
        sp2.almost_eq_epsilon(op2, epsilon)
    }
}

impl <T: Number> AlmostEq<T> for Ray<T> {
    fn almost_eq_epsilon(self, Ray(op1, op2): Ray<T>, epsilon: T) -> bool {
        let Ray(sp1, sp2) = self;
        sp1.almost_eq_epsilon(op1, epsilon) &&
        sp2.almost_eq_epsilon(op2, epsilon)
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
