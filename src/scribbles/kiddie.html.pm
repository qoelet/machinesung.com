#lang pollen

◊h1{Kiddie scripting}
◊h2{Scripting games with my 6 year old son}
◊m-article{
  ◊p{◊em{writing in progress, check back later.}}
  ◊m-code-lua{
    -- Example
    x=64
    y=64
    function _update()
      if (btn(0)) then x=x-1 end
      if (btn(1)) then x=x+1 end
      if (btn(2)) then y=y-1 end
      if (btn(3)) then y=y+1 end
    end

    function _draw()
      rectfill(0,0,127,127,5)
      circfill(x,y,7,14)
    end
  }
}
◊m-back
