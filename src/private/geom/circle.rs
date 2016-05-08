use super::*;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Circle<T: Number>(pub Point<T>, pub T);

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
		if radius_diff < T::zero() { return false; }

		let dx = sx - ox;
		let dy = sy - oy;
		let dst = dx * dx + dy * dy;
		let radius_sum = sr + or;
		return !(radius_diff * radius_diff < dst) && (dst < radius_sum * radius_sum);
    }
}

