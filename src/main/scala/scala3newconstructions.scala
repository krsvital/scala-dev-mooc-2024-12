object scala3newconstructions {
  
  def f(x: Int): Int = {
    x*2
  }
  
  def f_withbug():Int = {
    4/0
  }
  
  @main def test(): Unit = {
    var x = 4
    val xs = 1 :: 2 :: 3 :: Nil
    val ys = 4 :: 5 :: 6 :: Nil
    
    if x <0 then
      "sdf"
    else if x ==0 then
      "sdf"
    else
      "sdf"
      
    while x>0 do x = f(x)
    val x1 = if x > 5 then "asdf" else 1
    
    for 
      x <- xs
      y <- ys
    do  
      println(x+y)  
    
    try f_withbug()
    catch case ex: Exception => println("sdvfg")
  }

}
