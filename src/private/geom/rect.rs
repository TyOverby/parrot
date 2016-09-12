use super::*;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Rect<T: Number>(pub Point<T>, pub Point<T>);

impl <T: Number> Rect<T> {
    pub fn xywh(x: T, y: T, w: T, h: T) -> Rect<T> {
        let p = Point(x, y);
        let v = Vector(w, h);
        Rect(p, p + v)
    }

    pub fn top_left(self) -> Point<T> {
        self.0
    }

    pub fn bottom_right(self) -> Point<T> {
        self.1
    }

    pub fn bottom_left(self) -> Point<T> {
        let Point(x, _) = self.top_left();
        let Point(_, y) = self.bottom_right();
        Point(x, y)
    }

    pub fn top_right(self) -> Point<T> {
        let Point(_, y) = self.top_left();
        let Point(x, _) = self.bottom_right();
        Point(x, y)
    }

    pub fn top(self) -> T {
        let Point(_, y) = self.top_left();
        y
    }

    pub fn bottom(self) -> T {
        let Point(_, y) = self.bottom_right();
        y
    }

    pub fn left(self) -> T {
        let Point(x, _) = self.top_left();
        x
    }

    pub fn right(self) -> T {
        let Point(x, _) = self.bottom_right();
        x
    }

    pub fn set_left(self, x: T) -> Rect<T> {
        let Point(left, _) = self.top_left();
        let mov = Vector(x - left, T::zero());
        self.translate_v(mov)
    }

    pub fn set_right(self, x: T) -> Rect<T> {
        let Point(right, _) = self.top_right();
        let mov = Vector(x - right, T::zero());
        self.translate_v(mov)
    }

    pub fn set_top(self, y: T) -> Rect<T> {
        let Point(_, top) = self.top_right();
        let mov = Vector(T::zero(), y - top);
        self.translate_v(mov)
    }

    pub fn set_bottom(self, y: T) -> Rect<T> {
        let Point(_, bottom) = self.bottom_right();
        let mov = Vector(T::zero(), y - bottom);
        self.translate_v(mov)
    }

    pub fn set_width(self, w: T) -> Rect<T> {
        let Point(x, _) = self.top_left();
        let Point(_, ry) = self.bottom_right();
        Rect(self.top_left(), Point(x + w, ry))
    }

    pub fn set_height(self, h: T) -> Rect<T> {
        let Point(_, y) = self.top_left();
        let Point(rx, _) = self.bottom_right();
        Rect(self.top_left(), Point(rx, y + h))
    }

    pub fn null_at(p: Point<T>) -> Rect<T> {
        Rect(p, p)
    }

    pub fn width(self) -> T {
        let Rect(Point(left, _), Point(right, _)) = self;
        right - left
    }

    pub fn height(self) -> T {
        let Rect(Point(_, top), Point(_, bottom)) = self;
        bottom - top
    }

    pub fn area(self) -> T {
        self.width() * self.height()
    }

    pub fn constrain_to(self, other: Rect<T>) -> Rect<T> {
        let Point(x1, y1) = self.0;
        let Point(x2, y2) = other.0;
        let Point(x3, y3) = other.1;

        let mut xo = x1;
        let mut yo = y1;
        let wo = self.width();
        let ho = self.height();

        if x1 < x2 {
            xo = x2;
        }

        if y1 < y2 {
            yo = y2;
        }

        if xo + wo > x3 {
            xo = x3 - wo;
        }

        if yo + wo > y3 {
            yo = y3 - wo;
        }

        Rect::xywh(xo, yo, wo, ho)
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

impl <T: Number> Contains<Rect<T>> for Rect<T> {
    fn contains(self, Rect(p1, p2): Rect<T>) -> bool {
        self.contains(p1) && self.contains(p2)
    }
}

impl <T: Number> Bounded<T> for Rect<T> {
    fn aabb(self) -> Rect<T> {
        self
    }
}

impl <T: Number> AlmostEq<T> for Rect<T> {
    fn almost_eq_epsilon(self, Rect(op1, op2): Rect<T>, epsilon: T) -> bool {
        let Rect(sp1, sp2) = self;
        sp1.almost_eq_epsilon(op1, epsilon) &&
        sp2.almost_eq_epsilon(op2, epsilon)
    }
}

impl <N: Number> Translate<N> for Rect<N> {
    fn translate(self, x: N, y: N) -> Rect<N> {
        let Rect(p1, p2) = self;
        Rect(p1.translate(x, y), p2.translate(x, y))
    }
}
