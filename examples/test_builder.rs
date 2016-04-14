#![allow(dead_code, unused_variables)]

extern crate lux;
extern crate parrot;

use lux::prelude::*;
use lux::interactive::Event;
use parrot::geom::*;

#[derive(Copy, Clone, Debug)]
enum Geometry {
    Circle(Circle<f32>),
    Rect(Rect<f32>),
    Line(LineSegment<f32>),
    Point(Point<f32>),
}

#[derive(Copy, Clone, Debug)]
enum DrawGeom {
    Circle,
    Rect,
    Line,
    Point,
}

#[derive(Copy, Clone, Debug)]
enum Operation {
    Contains,
    Intersection
}

#[derive(Clone, Debug)]
enum CalcReturn {
    IntersectionPoints(Intersections<f32>)
}

struct Scene {
    window: Window,
    drawing: DrawGeom,
    operation: Operation,

    selected_a: Option<Geometry>,
    selected_b: Option<Geometry>,

    last_click: Option<(f32, f32)>,
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
        }
    }

    fn run(&mut self) {
        println!("[a][b] to clear item a or item b");
        println!("[p][l] to switch insertion to point or line");
        while self.window.is_open() {
            let events = self.window.events();
            let mous_pos = self.window.mouse_pos();
            let (should_redraw, should_calculate) = self.respond_to_input(events, mous_pos) ;
            if should_redraw {
                let frame = self.window.cleared_frame((1.0, 1.0, 1.0));
                self.draw(frame, mous_pos);
                if should_calculate {
                    self.calculate();
                }
            }
        }
    }

    fn calculate(&self) {
        let a = self.selected_a.unwrap();
        let b = self.selected_b.unwrap();

        match (self.operation, a, b) {
            (Operation::Intersection, Geometry::Line(l1), Geometry::Line(l2)) => {
                println!("intersection of {:?} and {:?} is {:?}", l1, l2, l1.intersects(l2));
            }
            (op, geo1, geo2) => {
                println!("could not compute {:?} for {:?} and {:?}", op, geo1, geo2);
            }
        }
    }

    // Returns true if any events were passed through
    fn respond_to_input(&mut self, events: EventIterator, mouse: (f32, f32)) -> (bool, bool) {
        let mut got_event = false;
        let mut got_input = false;
        for event in events {
            got_event = true;
            match event {
                Event::MouseUp(_) => {
                    got_input = self.input_point(mouse);
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
                Event::KeyPressed(_, Some('l'), _) => {
                    self.drawing = DrawGeom::Line;
                    self.last_click = None;
                }
                _ => {}
            }
        }
        (got_event, got_input)
    }

    // Returns true if the last object was just placed.
    fn input_point(&mut self, (x, y): (f32, f32)) -> bool {
        let &mut Scene { ref mut drawing, ref mut selected_a, ref mut selected_b, ref mut last_click, .. } = self;
        match (*drawing, selected_a, selected_b, last_click) {
            (_, &mut Some(_), &mut Some(_), last) => {
                *last = None;
                false
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
            _ => false
        }
    }

    fn draw(&mut self, mut frame: Frame, (mx, my): (f32, f32)) {
        draw_point(Point(mx, my), &mut frame);

        match self.selected_a {
            Some(Geometry::Point(p)) => {
                draw_point(p, &mut frame);
            }
            Some(Geometry::Line(l)) => {
                draw_line_segment(l, &mut frame);
            }
            Some(_) => {}
            None => {}
        }

        match self.selected_b {
            Some(Geometry::Point(p)) => {
                draw_point(p, &mut frame);
            }
            Some(Geometry::Line(l)) => {
                draw_line_segment(l, &mut frame);
            }
            Some(_) => {}
            None => {}
        }
    }
}

fn draw_point(Point(cx, cy): Point<f32>, frame: &mut Frame) {
    let r1 = 20.0;
    let r2 = 5.0;
    frame.circle(cx - r1, cy - r1, r1 * 2.0).color((0.5, 0.5, 0.5)).fill();
    frame.circle(cx - r2, cy - r2, r2 * 2.0).color((0.0, 0.0, 0.0)).fill();
}

fn draw_line_segment(LineSegment(p1, p2): LineSegment<f32>, frame: &mut Frame) {
    draw_point(p1, frame);
    draw_point(p2, frame);
    frame.draw_line(p1.0, p1.1, p2.0, p2.1, 2.0);
}

fn main() {
    let mut scene = Scene::new();
    scene.run();
}
