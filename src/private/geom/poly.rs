use super::*;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Poly<T: Number> {
    points: Vec<Point<T>>,
    aabb: Rect<T>,
}

pub struct LinesFromPolyIterator<'a, T: Number + 'a> {
    inner: ::std::slice::Windows<'a, Point<T>>,
    first_and_last: Option<(Point<T>, Point<T>)>,
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

    pub fn convex_hull(&self) -> Poly<T> {
        let points = self.points();
        let mut lowest = None;
        for point in points.iter().cloned() {
            lowest = match (lowest, point) {
                (None, point) => Some(point),
                (Some(p1), p2) => Some(if p1 < p2 {p1} else {p2})
            };
        }

        let mut on_hull = lowest.unwrap();
        let mut i = 0;
        let mut out = vec![];

        loop {
            out.push(on_hull);
            let mut endpoint = points[0];
            for j in 1 .. points.len() {
                if endpoint.almost_eq(on_hull) || (points[j] - out[i]).cross(endpoint - out[i]) < T::zero() {
                    endpoint = points[j];
                }
            }
            i += 1;
            on_hull = endpoint;
            if endpoint == out[0] { break; }
        }

        println!("cvh has {} points", out.len());
        Poly::new(out)
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

impl <'a, T: Number> Bounded<T> for &'a Poly<T> {
    fn aabb(self) -> Rect<T> {
        self.aabb
    }
}
