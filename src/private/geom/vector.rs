use super::*;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Vector<T: Number>(pub T, pub T);

impl <T: Number> Vector<T> {
    pub fn magnitude_2(self) -> T {
        let Vector(dx, dy) = self;
        dx * dx + dy * dy
    }

    pub fn magnitude(self) -> T {
        self.magnitude_2().sqrt()
    }

    pub fn cross(self, Vector(ox, oy): Vector<T>) -> T {
        self.0 * oy - self.1 * ox
    }

    pub fn dot(self, Vector(ox, oy): Vector<T>) -> T {
        self.0 * ox + self.1 * oy
    }

    pub fn angle_to(self, other: Vector<T>) -> T {
        (self.dot(other) / (self.magnitude() * self.magnitude())).acos()
    }
}

impl <T: Number> AlmostEq<T> for Vector<T> {
    fn almost_eq_epsilon(self, Vector(ox, oy): Vector<T>, epsilon: T) -> bool {
        let Vector(sx, sy) = self;
        sx.almost_eq_epsilon(ox, epsilon) &&
        sy.almost_eq_epsilon(oy, epsilon)
    }
}
