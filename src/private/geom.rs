pub trait Number: ::num::traits::Float + ::std::cmp::PartialOrd + Copy + ::std::fmt::Debug {}

pub trait DistanceTo<T, N: Number> {
    fn distance_squared(self, other: T) -> N;
}

pub trait CanContain<T> {
    fn contains(self, T) -> bool;
}

pub struct LinesFromPolyIterator<'a, T: Number + 'a> {
    inner: ::std::slice::Windows<'a, Point<T>>,
    first_and_last: Option<(Point<T>, Point<T>)>,
}

impl <T: ::num::traits::Float + ::std::cmp::PartialOrd + Copy + ::std::fmt::Debug> Number for T {}

pub fn distance_squared<A, B, N>(a: A, b: B) -> N
where A: DistanceTo<B, N>, N: Number {
    a.distance_squared(b)
}

pub fn distance<A, B, N>(a: A, b: B) -> N
where A: DistanceTo<B, N>, N: Number {
    distance_squared(a, b).sqrt()
}

pub fn contains<A, B>(a: A, b: B) -> bool
where A: CanContain<B> {
    a.contains(b)
}

#[derive(Debug, Copy, Clone)]
pub struct Point<T: Number>(pub T, pub T);
#[derive(Debug, Copy, Clone)]
pub struct Vector<T: Number>(pub T, pub T);
#[derive(Debug, Copy, Clone)]
pub struct LineSegment<T: Number>(pub Point<T>, pub Point<T>);
#[derive(Debug, Copy, Clone)]
pub struct Ray<T: Number>(pub Point<T>, pub Vector<T>);
#[derive(Debug, Copy, Clone)]
pub struct Rect<T: Number>(pub Point<T>, pub Point<T>);
#[derive(Debug, Clone)]
pub struct Poly<T: Number> {
    points: Vec<Point<T>>,
    aabb: Rect<T>,
}

impl <T: Number> Rect<T> {
    pub fn null_at(p: Point<T>) -> Rect<T> {
        Rect(p, p)
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

impl <'a, T: Number> CanContain<Point<T>> for Rect<T> {
    fn contains(self, Point(px, py): Point<T>) -> bool {
        let Rect(Point(tlx, tly), Point(brx, bry)) = self;
        px >= tlx && px < brx && py >= tly && py < bry

    }
}

impl <'a, T: Number> CanContain<Point<T>> for &'a Poly<T> {
    fn contains(self, p: Point<T>) -> bool {
        if !contains(self.aabb, p) { return false; }

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

impl <T: Number> DistanceTo<Point<T>, T> for Point<T> {
    fn distance_squared(self, other: Point<T>) -> T {
        let dx = self.0 - other.0;
        let dy = self.1 - other.1;
        dx * dx + dy * dy
    }
}

impl <T:Number> DistanceTo<LineSegment<T>, T> for Point<T> {
    fn distance_squared(self, LineSegment(v, w): LineSegment<T>) -> T {
        use ::num::{Zero, One};
        let l2 = distance_squared(v, w);
        if l2 == Zero::zero() { //  TODO: epsilon
            return distance_squared(self, v);
        }
        let t = ((self.0 - v.0) * (w.0 - v.0) + (self.1 - v.1) * (w.1 - v.1)) / l2;
        if t < Zero::zero() {
            distance_squared(self, v)
        } else if t > One::one() {
            distance_squared(self, w)
        } else {
            distance_squared(self, Point (
                v.0 + t * (w.0 - v.0),
                v.1 + t * (w.1 - v.1)
            ))
        }

    }
}

impl <T: Number> DistanceTo<Point<T>, T> for LineSegment<T> {
    fn distance_squared(self, point: Point<T>) -> T {
        distance_squared(point, self)
    }
}

impl <'a, T: Number> DistanceTo<&'a Poly<T>, T> for Point<T> {
    fn distance_squared(self, polygon: &Poly<T>) -> T {
        let mut min_dist = T::infinity();
        for line in polygon.lines() {
            min_dist = min_dist.min(distance_squared(self, line));
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
