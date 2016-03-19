-- install supercollider, then the easiest way to start the server is to open the app and hit ctrl-b

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}

import Vivid
import Saraswati

song = horizontal $ map (TemporalUnit. uncurry Note)
  [((C,5), 0.25), ((D,5), 0.25), ((E,5), 0.5), ((C,5), 0.25), ((D,5), 0.25), ((E,5), 0.5),
   ((E,5), 0.25), ((F,5), 0.25), ((E,5), 0.25), ((D,5), 0.25), ((C,5), 0.5), ((G,4), 0.5)]

main = do
  --installHandler sigINT (\s -> (print "hi"))
  renderMedia (song :=: transpose 3 song)
  wait 10000
  return ()
