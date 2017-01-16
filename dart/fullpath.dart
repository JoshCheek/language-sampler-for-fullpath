class Robot {
  int x, y;
  Robot([this.x=0, this.y=0]);
  String get location => "($x,$y)";

  moveRight() { x++; }
  moveLeft()  { x--; }
  moveUp()    { y++; }
  moveDown()  { y--; }
}

main() {
  print("*** Robot Stuffs ***");
  var r = new Robot();
  print("Robot starts at: " + r.location);

  r.moveRight();
  r.moveRight();
  r.moveRight.call();
  (){ r.moveRight(); }();
  var m = r.moveRight;
  m();
  r ..moveRight()
    ..moveRight();

  print("---");
  print("Robot ends at: " + r.location);
}
