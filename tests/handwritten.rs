extern crate parrot;
use parrot::geom::*;

#[test]
fn colinear_line_segment_intersection() {
    // line below
    let la = LineSegment(Point(0.0, 0.0), Point(5.0, 0.0));
    let lb = LineSegment(Point(1.0, 1.0), Point(3.0, 1.0));
    assert!(la.intersects(lb) == Intersections::None);
    let (la, lb) = (lb, la);
    assert!(la.intersects(lb) == Intersections::None);

    // line above
    let la = LineSegment(Point(0.0, 0.0), Point(5.0, 0.0));
    let lb = LineSegment(Point(1.0, -1.0), Point(3.0, -1.0));
    assert!(la.intersects(lb) == Intersections::None);
    let (la, lb) = (lb, la);
    assert!(la.intersects(lb) == Intersections::None);

    // In the middle
    let la = LineSegment(Point(0.0, 0.0), Point(5.0, 0.0));
    let lb = LineSegment(Point(1.0, 0.0), Point(3.0, 0.0));
    assert!(la.intersects(lb) == Intersections::Two(lb.0, lb.1));
    let (la, lb) = (lb, la);
    assert!(lb.intersects(la) == Intersections::Two(la.0, la.1));

    // Making a longer line
    let la = LineSegment(Point(0.0, 0.0), Point(3.0, 0.0));
    let lb = LineSegment(Point(3.0, 0.0), Point(5.0, 0.0));
    assert!(la.intersects(lb) == Intersections::None);
    let (la, lb) = (lb, la);
    assert!(la.intersects(lb) == Intersections::None);
}
