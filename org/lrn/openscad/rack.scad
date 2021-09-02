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
    translate([0,120,120])
        rotate([0,-90]) {
            difference() {
                linear_extrude(d) 
                    square([sz, sz], center=true);
                translate([125,125,-10])
                    rotate([0,0,45]) 
                        linear_extrude(d+20) 
                            square([sz, sz], center=true);
                
                }
            }
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

module flank(w=tube_square_size) {
    rotate([0,-90,0]) {
        linear_extrude(sheet_thickness)
            square([240, 80], center=true);
        translate([-w, w-w, (pin_length/4)]) { Pin(); }
        translate([w, w-w, (pin_length/4)]) { Pin(); }
    }
}


module horbar(w=tube_square_size, bh=bar_height) {
    translate([w+sheet_thickness, w/2, bh]) {
        flank();
        rotate([0,90]) {
            linear_extrude(width-w-sheet_thickness*2)
                circle(16);
        }
        translate([width-w-sheet_thickness*2,0,0])
            rotate([0,180]) {
                flank();
            }
    }
}


Frame();
translate([width,0,0])
    Frame();

horbar();