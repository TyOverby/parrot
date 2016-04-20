#![allow(dead_code, unused_variables)]

extern crate lux;
extern crate parrot;
extern crate sha1;
extern crate latin;

use lux::prelude::*;
use lux::color::{RED, GREEN, BLUE, BLACK};
use lux::interactive::Event;
use parrot::geom::*;

#[derive(Clone)]
enum Geometry {
    Circle(Circle<f32>),
    Rect(Rect<f32>),
    Line(LineSegment<f32>),
    Point(Point<f32>),
    Polygon(Poly<f32>),
}

#[derive(Copy, Clone, Debug)]
enum DrawGeom {
    Circle,
    Rect,
    Line,
    Point,
    Polygon
}

#[derive(Copy, Clone, Debug)]
enum Operation {
    Contains,
    Intersection
}

#[derive(Clone, Debug)]
enum CalcReturn {
    IntersectionPoints(Geometry, Geometry, Intersections<f32>),
    DoesContain(Geometry, Geometry, bool),
}

struct Scene {
    window: Window,
    drawing: DrawGeom,
    operation: Operation,

    selected_a: Option<Geometry>,
    selected_b: Option<Geometry>,

    last_click: Option<(f32, f32)>,
    last_calculation_result: Option<CalcReturn>,
}

impl Scene {
    fn new() -> Scene {
        Scene {
            window: Window::new_with_defaults().unwrap(),
            drawing: DrawGeom::Point,
            operation: Operation::Intersection,
            selected_a: None,
            selected_b: None,
            last_click: None,
            last_calculation_result: None,
        }
    }

    fn run(&mut self) {
        println!("[a][b] to clear item a or item b");
        println!("[p][l][c] to switch insertion between [point, line, circle]");
        println!("[k][i] to switch mode between [contains, intersection]");
        while self.window.is_open() {
            let events = self.window.events();
            let mous_pos = self.window.mouse_pos();
            let (should_redraw, should_calculate) = self.respond_to_input(events, mous_pos) ;
            if should_redraw {
                let frame = self.window.cleared_frame((1.0, 1.0, 1.0));
                if should_calculate {
                    self.last_calculation_result = self.calculate();
                }
                self.draw(frame, mous_pos);
            }
        }
    }

    fn calculate(&self) -> Option<CalcReturn> {
        let (a, b) = match (self.selected_a.clone(), self.selected_b.clone()) {
            (Some(a), Some(b)) => (a, b),
            _ => return None,
        };

        match (self.operation, a.clone(), b.clone()) {
            (Operation::Intersection, Geometry::Line(l1), Geometry::Line(l2)) => {
                let result = CalcReturn::IntersectionPoints(a, b, l1.intersects(l2));
                println!("intersection of {:?} and {:?} is {:?}", l1, l2, l1.intersects(l2));
                Some(result)
            }
            (Operation::Intersection, Geometry::Circle(c), Geometry::Line(l)) => {
                let result = CalcReturn::IntersectionPoints(a, b, c.intersects(l));
                println!("intersection of {:?} and {:?} is {:?}", l, c, l.intersects(c));
                Some(result)
            }
            (Operation::Intersection, Geometry::Line(l), Geometry::Circle(c)) => {
                let result = CalcReturn::IntersectionPoints(a, b, c.intersects(l));
                println!("intersection of {:?} and {:?} is {:?}", l, c, l.intersects(c));
                Some(result)
            }
            (Operation::Contains, Geometry::Circle(c1), Geometry::Circle(c2)) => {
                let result = c1.contains(c2);
                println!("contains of {:?} and {:?} is {:?}", c1, c2, result);
                Some(CalcReturn::DoesContain(a, b, result))
            }
            (Operation::Contains, Geometry::Circle(c), Geometry::Point(p)) => {
                let result = c.contains(p);
                println!("contains of {:?} and {:?} is {:?}", c, p, result);
                Some(CalcReturn::DoesContain(a, b, result))
            }
            (op, geo1, geo2) => {
                println!("could not compute {:?} for {:?} and {:?}", op, geo1, geo2);
                None
            }
        }
    }

    // Returns true if any events were passed through
    fn respond_to_input(&mut self, events: EventIterator, mouse: (f32, f32)) -> (bool, bool) {
        let mut got_event = false;
        let mut should_calculate = false;
        for event in events {
            got_event = true;
            match event {
                Event::MouseUp(_) => {
                    should_calculate  = self.input_point(mouse);
                }
                Event::KeyPressed(_, Some('a'), _) => {
                    self.selected_a = None;
                }
                Event::KeyPressed(_, Some('b'), _) => {
                    self.selected_b = None;
                }
                Event::KeyPressed(_, Some('p'), _) => {
                    self.drawing = DrawGeom::Point;
                    self.last_click = None;
                }
                Event::KeyPressed(_, Some('o'), _) => {
                    self.drawing = DrawGeom::Polygon;
                    self.last_click = None;
                }
                Event::KeyPressed(_, Some('c'), _) => {
                    self.drawing = DrawGeom::Circle;
                    self.last_click = None;
                }
                Event::KeyPressed(_, Some('l'), _) => {
                    self.drawing = DrawGeom::Line;
                    self.last_click = None;
                }
                Event::KeyPressed(_, Some('i'), _) => {
                    self.operation = Operation::Intersection;
                    should_calculate = true;
                }
                Event::KeyPressed(_, Some('k'), _) => {
                    self.operation = Operation::Contains;
                    should_calculate = true;
                }
                Event::KeyPressed(_, Some(' '), _) => {
                    self.produce_test_case();
                }
                _ => {}
            }
        }
        (got_event, should_calculate)
    }

    // Returns true if the last object was just placed.
    fn input_point(&mut self, (x, y): (f32, f32)) -> bool {
        let &mut Scene { ref mut drawing, ref mut selected_a, ref mut selected_b, ref mut last_click, .. } = self;
        match (*drawing, selected_a, selected_b, last_click) {
            (_, &mut Some(_), &mut Some(_), last) => {
                *last = None;
                false
            }
            (DrawGeom::Polygon, a@&mut None, _, _) => {
                let p = Point(x, y);
                *a = Some(Geometry::Polygon(Poly::new(vec![p, p, p])));
                true
            }
            (DrawGeom::Polygon, &mut Some(Geometry::Polygon(ref mut p)), _, _) => {
                p.add_point(Point(x, y));
                if p.points().len() == 5 {
                    p.dedupe_consecutive_points();
                }
                true
            }

            (DrawGeom::Point, a@&mut None, b, _) => {
                *a = Some(Geometry::Point(Point(x, y)));
                b.is_some()
            }
            (DrawGeom::Point, &mut Some(_), b@&mut None , _) => {
                *b = Some(Geometry::Point(Point(x, y)));
                true
            }

            (DrawGeom::Line, _, _, l@&mut None) => {
                *l = Some((x, y));
                false
            }
            (DrawGeom::Line, a@&mut None, b, l@&mut Some(_)) => {
                let (lx, ly) = l.unwrap();
                *a = Some(Geometry::Line(LineSegment(Point(x, y), Point(lx, ly))));
                *l = None;
                b.is_some()
            }
            (DrawGeom::Line, &mut Some(_), b@&mut None , l@&mut Some(_)) => {
                let (lx, ly) = l.unwrap();
                *b = Some(Geometry::Line(LineSegment(Point(x, y), Point(lx, ly))));
                *l= None;
                true
            }

            (DrawGeom::Circle, _, _, l@&mut None) => {
                *l = Some((x, y));
                false
            }
            (DrawGeom::Circle, a@&mut None, b, l@&mut Some(_)) => {
                let (lx, ly) = l.unwrap();
                let center = Point(lx, ly);
                let edge = Point(x, y);
                *a = Some(Geometry::Circle(Circle(center, center.distance(edge))));
                *l = None;
                b.is_some()
            }
            (DrawGeom::Circle, &mut Some(_), b@&mut None , l@&mut Some(_)) => {
                let (lx, ly) = l.unwrap();
                let center = Point(lx, ly);
                let edge = Point(x, y);
                *b = Some(Geometry::Circle(Circle(center, center.distance(edge))));
                *l = None;
                true
            }
            _ => false
        }
    }

    fn draw(&mut self, mut frame: Frame, (mx, my): (f32, f32)) {
        draw_point(Point(mx, my), &mut frame, BLACK);

        fn draw_selected(selected: Option<&Geometry>, frame: &mut Frame, color: [f32; 4]) {
            match selected {
                Some(&Geometry::Point(p)) => {
                    draw_point(p, frame, color);
                }
                Some(&Geometry::Line(l)) => {
                    draw_line_segment(l, frame, color);
                }
                Some(&Geometry::Circle(c)) => {
                    draw_circle(c, frame, color);
                }
                Some(&Geometry::Polygon(ref p)) => {
                    draw_polygon(p, frame, color);
                }
                Some(_) => {}
                None => {}
            }
        }

        draw_selected(self.selected_a.as_ref(), &mut frame, RED);
        draw_selected(self.selected_b.as_ref(), &mut frame, GREEN);

        if self.selected_a.is_some() && self.selected_b.is_some() {
            match self.last_calculation_result.as_ref() {
                Some(&CalcReturn::IntersectionPoints(_, _, Intersections::None)) => { }
                Some(&CalcReturn::IntersectionPoints(_, _, Intersections::One(a))) => {
                    draw_point(a, &mut frame, BLUE);
                }
                Some(&CalcReturn::IntersectionPoints(_, _, Intersections::Two(a, b))) => {
                    draw_point(a, &mut frame, BLUE);
                    draw_point(b, &mut frame, BLUE);
                }
                Some(&CalcReturn::IntersectionPoints(_, _, Intersections::Many(ref all))) => {
                    for &p in all {
                        draw_point(p, &mut frame, BLUE);
                    }
                }
                Some(&CalcReturn::DoesContain(_, _, b)) => {
                    frame.square(0.0, 0.0, 50.0).color(if b {GREEN} else {RED}).fill();
                }
                None => {}
            }
        }
    }

    fn produce_test_case(&self) {
        const EPSILON: &'static str = "0.0000001f32";
        let assertion = match self.last_calculation_result.clone() {
            Some(CalcReturn::IntersectionPoints(a, b, pts)) => {
                format!("assert!({:.20?}.intersects({:.20?}).almost_eq_epsilon({:?}, {}));", a, b, pts, EPSILON)
                //format!("println!(\"{{:.20?}}\", {:.20?}.intersects({:.20?})); println!(\"{{:.20?}}\", {:.20?});", a, b, pts)
            }

            Some(CalcReturn::DoesContain(a, b, c)) => {
                format!("assert!({:.20?}.contains({:.20?}).almost_eq_epsilon({:?}, {}));", a, b, c, EPSILON)
            }

            None => {
                println!("no last calculation result");
                return;
            }
        };

        let mut sha1 = ::sha1::Sha1::new();
        sha1.update(assertion.as_bytes());
        let output = format!("#[test]\nfn test_{}() {{\n    {}\n}}\n", sha1.hexdigest(), assertion);
        println!("{}", output);
        ::latin::file::append("./tests/guigen.rs", output).unwrap();
    }
}

fn draw_point(Point(cx, cy): Point<f32>, frame: &mut Frame, color: [f32; 4]) {
    let r1 = 20.0;
    let r2 = 5.0;
    let r = color[0];
    let g = color[1];
    let b = color[2];
    let a = color[3];
    frame.circle(cx - r1, cy - r1, r1 * 2.0).segments(20).color((r, g, b, a / 2.0)).fill();
    frame.circle(cx - r2, cy - r2, r2 * 2.0).segments(10).color((r, g, b, a)).fill();
}

fn draw_circle(Circle(Point(cx, cy), r): Circle<f32>, frame: &mut Frame, color: [f32; 4]) {
    let mut color = color;
    color[3] /= 2.0;
    frame.circle(cx - r, cy - r, r * 2.0).color(color).fill();
}

fn draw_line_segment(LineSegment(p1, p2): LineSegment<f32>, frame: &mut Frame, color: [f32; 4]) {
    draw_point(p1, frame, color);
    draw_point(p2, frame, color);
    frame.with_color(color, |frame| frame.draw_line(p1.0, p1.1, p2.0, p2.1, 2.0));
}

fn draw_polygon(poly: &Poly<f32>, frame: &mut Frame, color: [f32; 4]) {
    for line in poly.lines() {
        draw_line_segment(line, frame, color);
    }
}

impl ::std::fmt::Debug for Geometry {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &Geometry::Circle(Circle(Point(cx, cy), r)) =>
                write!(f, "Circle(Point({:.20}, {:.20}), {:.20})", cx, cy, r),
            &Geometry::Rect(Rect(Point(x1, y1), Point(x2, y2))) =>
                write!(f, "Rect(Point({:.20}, {:.20}), Point({:.20}, {:.20}))", x1, y1, x2, y2),
            &Geometry::Line(LineSegment(Point(x1, y1), Point(x2, y2))) =>
                write!(f, "LineSegment(Point({:.20}, {:.20}), Point({:.20}, {:.20}))", x1, y1, x2, y2),
            &Geometry::Point(Point(x, y)) =>
                write!(f, "Point({:.20}, {:.20})", x, y),
            &Geometry::Polygon(ref poly) => {
                try!(write!(f, "Polygon::new(vec!["));
                for &Point(x, y) in poly.points() {
                    try!(write!(f, "Point({:.20}, {:.20})", x, y));
                }
                write!(f, "])")
            }

        }
    }
}

fn main() {
    let mut scene = Scene::new();
    scene.run();
}

