union () {
  union () {
    translate ([0, 7.9, 1]) {
      cube ([20.2, 2.4, 2], center=true);
    }
    union () {
      translate ([9.35, 0, 1]) {
        cube ([1.5, 17.8, 2], center=true);
      }
      translate ([8.5, 0, 1.45]) {
        cube ([1.5, 17.8, 1.1], center=true);
      }
    }
  }
  mirror ([0, 1, 0]) {
    mirror ([1, 0, 0]) {
      union () {
        translate ([0, 7.9, 1]) {
          cube ([20.2, 2.4, 2], center=true);
        }
        union () {
          translate ([9.35, 0, 1]) {
            cube ([1.5, 17.8, 2], center=true);
          }
          translate ([8.5, 0, 1.45]) {
            cube ([1.5, 17.8, 1.1], center=true);
          }
        }
      }
    }
  }
}
