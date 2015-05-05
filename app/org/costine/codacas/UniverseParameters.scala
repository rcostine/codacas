package org.costine.codacas

class UniverseParameters {
  // ship will have this quantity of fooples to start
  var initialFooples = 4000
  
  // ship will have this amount of WQL to start
  var initialWql : Double = 4000.0
  
  // ship's fooples will recharge at these many per sec
  var foopleRechargeRate:Double = 3.0
  
  // ship's wql will recharge at these many per sec
  // with a multiplier based of the distance to the pushed up against
  // star, if one is contained in the ship.
  var initialWqlRR:Double = 30.0

  // minimum wql to allow movement
  var wqlReqForMove = 1000.0

  // dropoff these many wqls per covered units
  var wqlDropoffFactor: Double = 10.0
  
  // ship's max fooples will be this number
  var initialMaxFooples = 4000
  
  // phaser fooples times this value will be the amount of fooples that
  // the ship will use to fire the phasers. eg. with it set to .5, a PA100
  // will subtract 50 fooples from the ship. It's the cost of firing them.
  var phaserFoopleFactor:Double = 0.5
  
  // phasers will dropoff at this rate * the distance.
  var phaserDropoffFactor:Double = .1
  
  // ship will start with this amount of torps. Needs to be a Double since we can
  // recharge something like .5, or .3 of a torp per sec (one every 2 seconds). 
  // Even so, we can't fire a half a torp.  torps must be >= 1 for them to deploy.
  var initShipTorps = 50.0 
  
  // ship will have these number of maximum torps
  var initMaxShipTorps = 50.0
  
  // torp capacity will be replenished at one roughly every ten secs
  var initTorpReplenishRate = .1
  
  // can't fire torp if payload is gt than this fraction 0 - 1 of current fooples
  // (in this case - half)
  var torpFirePct = 0.5
  
  // initial torp size
  var torpSize = 2.0
  
  // initial torp mass
  var torpMass = 0.00001
  
  // ship will have these number of maximum mines
  var initMaxShipMines = 10
  
  // ship available mines will be replaced at these number per second 
  // up to the max.
  var initMineReplenishRate = .1
  
  // ship available mines won't start to get replaced until the ships
  // fooples reach this number.
  var minMineReplenishFpl = 2000

  // W command magnitude multiplier
  // When specifying numbers in W command they will be scaled appropriately by
  // multiplying by this value, by default.
  var wCommandMagnitudeFactor = 0.001
 
}
