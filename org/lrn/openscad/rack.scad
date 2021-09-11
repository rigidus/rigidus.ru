height = 2400;
depth  = 1400;
width  = 1300;
tube_square_size = 80;
tube_thickness  = 3;
sheet_thickness = 2;
pin_length = 25+sheet_thickness*2+80;
bar_height = 2000;

module SquareTube(h=height, w=tube_square_size, xyz=[0,0,0]) {
    TT = tube_thickness;
    color([0.5, 0.5, 0.5, 0.5])
    translate(xyz) {
        translate([w/2, w/2, 0]) {
            difference () {
                linear_extrude(height=h) 
                    square([w, w], center=true);
                translate([0, 0, -5])
                    linear_extrude(height=h+6)
                        square([w-TT, w-TT], center=true);
            }
        }
    }
}

module Frame(h=height, d=depth, w=tube_square_size) {
    SquareTube();
    SquareTube(xyz=[0,d+w,0]);
    rotate([-90]) {
        SquareTube(h=d, xyz=[0,-w,w]);
        SquareTube(h=d, xyz=[0,-h,w]);
    }
    QuadLock();
}

module Pin(len=pin_length) {
    color([0.8, 0.2, 0.1, 0.5])
    translate([0,0,-(len/2-15)]) 
        linear_extrude(height=pin_length)
            circle(d=16);
}

module Lock(sz=240, d=sheet_thickness) {
    color([0.5, 0.5, 0.2, 0.5])
        polyhedron(
            points=[ [0,0,0],[0,0,320],[0,320,0],
                     [-d,0,0],[-d,0,320],[-d,320,0]
                    ],
            faces=[[0,1,2],[3,4,5], [0,1,4,3], [1,2,5,4], [2,0,3,5]
                    ]
        );
}

module DoubleLock(w=tube_square_size) {
    Lock();
    translate([tube_square_size+sheet_thickness,0,0]) {
        Lock();
    }
    // pins
    translate([0,120,120])
        rotate([0,-90]) {
            translate([w,-w,-(pin_length/2)]) { Pin(); }
            translate([-w,w,-(pin_length/2)]) { Pin(); }
            translate([-w,-w,-(pin_length/2)]) { Pin(); }
            translate([w-w,-w,-(pin_length/2)]) { Pin(); }
            translate([-w,w-w,-(pin_length/2)]) { Pin(); }
        }
}

module PairLock(w=tube_square_size) {
    DoubleLock();
    rotate(180)
        translate([-w,-depth-w*2,0])
        DoubleLock();
    
}

module QuadLock(w=tube_square_size) {
    PairLock();
    translate([0,depth+w*2,height])
        rotate([180,0,0])
            PairLock();
}

module Flank(w=tube_square_size) {
    rotate([0,-90,0]) {
        linear_extrude(sheet_thickness)
            square([240, 80], center=true);
        translate([-w, w-w, (pin_length/4)]) { Pin(); }
        translate([w, w-w, (pin_length/4)]) { Pin(); }
    }
}


module Horbar(w=tube_square_size, bh=bar_height) {
    translate([w+sheet_thickness, w/2, bh]) {
        Flank();
        rotate([0,90]) {
            linear_extrude(width-w-sheet_thickness*2)
                circle(16);
        }
        translate([width-w-sheet_thickness*2,0,0])
            rotate([0,180]) {
                Flank();
            }
    }
}

module Barbell () {
    translate([-350, 1450, 1000]) {
        rotate([0,90]) {
            linear_extrude(2020)
            circle(16);
        }
    }
}

module Stand() {
    translate([40, depth+78, 970]) {
        rotate([0,0,270]) {
            Flank();
            rotate([0,-90,0]) {
                translate([0, 0, (pin_length/4)]) { 
                    color([0.2, 0.2, 0.1, 0.5])
                        translate([0,0,-80]) 
                            linear_extrude(height=190)
                                circle(d=28);
                    color([0.2, 0.2, 0.1, 0.5])
                        translate([-20, -20, -80-sheet_thickness]) { 
                            linear_extrude(sheet_thickness)
                                square(40);
                        }
                }
            }
        }
    }
}

module Table() {
    color([0.7, 0.8, 0.3, 0.5])
    translate([tube_square_size,tube_square_size,740])
        linear_extrude(sheet_thickness)
                square([width-tube_square_size, depth]);
}

module Leg(len, rot=[0,0,0], trs=[0,0,0]) {
    translate(trs)
        rotate(rot) 
            linear_extrude(len)
                square([20, 20]);
}


module Leg1 (d=depth) {
    hh=400;
    Leg(hh, [0,0,0], [550,0,0]);
    Leg(hh, [0,0,0], [550,d,0]);
    Leg(d, [-90,0,0], [550,0,100]);
    Leg(d, [-90,0,0], [550,0,hh]);
}

module Leg2 (d=depth) {
    Leg1(d);
    translate([300,0,0])
        Leg1(d);
    hh=400;
    ww=300;
    dd=550;
    Leg(ww, [0,90,0], [dd,0,hh]);
    Leg(ww, [0,90,0], [dd,d,hh]);
    Leg(ww, [0,90,0], [dd,0,100]);
    Leg(ww, [0,90,0], [dd,d,100]);
}
Leg2(depth);


Frame();
translate([width,0,0])
    Frame();
Horbar();
Barbell();
Stand();
Table();


