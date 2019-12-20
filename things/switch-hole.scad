union () {
  union () {
    translate ([0, 7.9, 2]) {
      cube ([20.2, 2.4, 4], center=true);
    }
    union () {
      translate ([9.35, 0, 2]) {
        cube ([1.5, 17.8, 4], center=true);
      }
      translate ([8.5, 0, 3.45]) {
        cube ([1.5, 17.8, 1.1], center=true);
      }
    }
  }
  mirror ([0, 1, 0]) {
    mirror ([1, 0, 0]) {
      union () {
        translate ([0, 7.9, 2]) {
          cube ([20.2, 2.4, 4], center=true);
        }
        union () {
          translate ([9.35, 0, 2]) {
            cube ([1.5, 17.8, 4], center=true);
          }
          translate ([8.5, 0, 3.45]) {
            cube ([1.5, 17.8, 1.1], center=true);
          }
        }
      }
    }
  }
}
