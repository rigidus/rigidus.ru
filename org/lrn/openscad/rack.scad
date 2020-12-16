module SquareTube80(h=2140, w=80, xyz=[0,0,0]) {
    color([0.5, 0.5, 0.5, 0.5])
    translate(xyz) {
        translate([w/2, w/2, 0]) {
            difference () {
                linear_extrude(height=h) 
                    square([w, w], center=true);
                translate([0, 0, -5])
                    linear_extrude(height=h+10)
                        square([w-3, w-3], center=true);
            }
        }
    }
}

module tube25(h=40) {
    rotate([0,90]) {
        difference() {
            linear_extrude(height=30)
                circle(d=33,5);
            translate([0, 0, -5])
                linear_extrude(height=40)
                    circle(d=27,1);
        }
    }
}

module hole25(h=40) {
    color("blue")
    rotate([0,90]) {
        linear_extrude(height=h)
            circle(d=33,5);
    }
}

module flank(h=240, w=80, xyz=[0,0,0]) {
    translate(xyz)
        translate([3,0,0])
            rotate([0,-90]) 
                linear_extrude(height=3) 
                    square([h, w]);
}


module flanker() {
    #flank(h=240, xyz=[0,0,0]);

    translate([-5, 40, 40])
        hole25(h=10);
    translate([-5, 40, 40+80*2])
        hole25(h=10);
}

module horbar() {
    flanker();
    translate([3, 0, 240/3])
        translate([0, 0, 80])
            rotate([0, 90])
                SquareTube80(h=1070, xyz=[0,0,0]);
    translate([1070+3, 0, 0])
        flanker();
}

SquareTube80(xyz=[0,0,0]);
translate([80, 0, 0])
    horbar();
SquareTube80(xyz=[80+3+1070+3, 0, 0]);
