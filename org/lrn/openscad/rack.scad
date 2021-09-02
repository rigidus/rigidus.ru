module SquareTube(h=2400, w=80, xyz=[0,0,0]) {
    color([0.5, 0.5, 0.5, 0.5])
    translate(xyz) {
        translate([w/2, w/2, 0]) {
            difference () {
                linear_extrude(height=h) 
                    square([w, w], center=true);
                translate([0, 0, -5])
                    linear_extrude(height=h+6)
                        square([w-3, w-3], center=true);
            }
        }
    }
}

module Bar(h=2400, d=1400, w=80) {
    SquareTube();
    SquareTube(xyz=[0,d+w,0]);
    rotate([-90]) {
        SquareTube(h=d, xyz=[0,-80,80]);
        SquareTube(h=d, xyz=[0,-h,80]);
    }
}

module Lock(sz=240, d=3) {
    color([0.5, 0.5, 0.2, 0.5])
    translate([0,120,120])
        rotate([0,-90]) 
            difference() {
                linear_extrude(d) 
                    square([sz, sz], center=true);
                translate([125,125,0])
                    rotate([0,0,45]) 
                        linear_extrude(d) 
                            square([sz, sz], center=true);
                }
}

module DoubleLock() {
    Lock();
    translate([80+3,0,0])
        Lock();
}

module PairDoubleLock() {
    DoubleLock();
    rotate(180,0,0)
    translate([-80,-1400-80*2,0])
        DoubleLock();
}

module QuadLock() {
    PairDoubleLock();
    translate([0,1400+80*2,2400])
        rotate([180,0,0])
            PairDoubleLock();
}


Bar();
translate([1300,0,0])
    Bar();
QuadLock();

translate([1300,0,0])
    QuadLock();



// ---------------------------------------


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

//SquareTube80(xyz=[0,0,0]);
//rotate(a=[-90,0,0]) {
    //SquareTube80(xyz=[0,2400,0]);
    //SquareTube80(h=1500, xyz=[-(2400),0,80]);
//}
    

//translate([80, 0, 0])
//    horbar();
//SquareTube80(xyz=[80+3+1070+3, 0, 0]);
