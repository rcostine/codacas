package org.costine.codacas.testers

import org.costine.codacas._
import org.costine.codacas.renderers._

object UniverseDump  {
	
	private var currentShip:Ship = null
	def usingShip (_ship:Ship) = {
	  currentShip = _ship
	}
	def usingShip = {
	  currentShip
	}
 
	private var uni:Universe = null
	def universe (_u:Universe) = {
	  uni = _u
	}
	def universe = {
	  uni
	}
 
	def newUniverse = {
		var starFactory = new StarFactory(.02, 
	                  1, 3,
	                  1000, 2000,
	                  50, 4000
						)
	 
		universe(new Universe(
				   200, 	// width 
				   200, 	// height
				   new UniverseParameters (),
	               starFactory, // how the universe populates stars
	               5,						// stargate size (the size of this universe in the parent)
	               new Coordinate (0,0), 	// stargate location (loc of universe in parent)
	               null						// parent universe 
		))
	 
		// (re)-populate all of the stars 
		universe.repopulateStars
  
		universe
	}
 

 
	def cmdUse (s:String) = {
	  val sh = universe.shipByName(s)
	   if (sh != null) {
		  usingShip (sh)
		  "Using ship " + sh.name 
	   }
	   else {
		  showShipsOrLogonMsg
	   }  
	}
 
	def cmdGalacticScan() = {
		val s = new CharacterArrayRenderer (this.universe, 20, " ", "\n")
		s.toString
    }
      
	def cmdShortScan() = {
		if (usingShip != null) {
			new ShortRangeCharacterArrayRenderer (usingShip, 40, 11, " ", "\n").toString + "\n" + usingShip
		}
		else {
			 showShipsOrLogonMsg
		}
	}
	def showShipIds = {
		(
			for {
				s <- universe.ships
				n = s.name
				} yield n
		) mkString ","
    }
 
	def showShipsOrLogonMsg = {
		if (showShipIds.size > 0) {
			"U[Ship], where Ship is: " + showShipIds
		}
		else {
			"\"logon\" to use a ship"
		}
	}
 
	def response(cmd:String) = {
		  var action = "S"
		  
		  if (cmd.length > 0) {
			  action = cmd.substring(0,1)
		  }
    
		  if (cmd == "LOGOFF") {
			  if (usingShip != null) {
				  if (this.universe.remove(usingShip)) {
					  if (this.universe.ships.length > 0) {
					    usingShip.name + " is off.\n" + cmdUse(this.universe.ships.apply(0).name)
					  }
				  }
				  usingShip(null) // no longer using a ship
			  }
			  else {
				 showShipsOrLogonMsg
			  }
		  }
		  else if (cmd == "LOGON") {
		    val s = this.universe.newShip()
		    if (s != null) {
		      cmdUse(s.name)
		    }
		    else {
		      "Game full. Try again later."
		    }  
		  }
		  else {
			  if (action == "U") {
				  if (cmd.length > 1) {
					   val s = cmd.substring(1,2)
					   cmdUse(s)
				  }
				  else {
				    if (usingShip != null) {
				      usingShip.name + " is on. Ships: " + showShipIds
				    }
				    else {
				    	showShipsOrLogonMsg
					}
				  }   
			  }
			  else if (action == "S")  {
			    cmdShortScan
			  }
			  else if (action == "V")  {
			    usingShip.name + ": " +usingShip
			  }
			  else if (action == "G")  {
				  cmdGalacticScan
			  }
			  else if (action == "H")  {
				  new HyperSpacer (this.universe).place(usingShip)
				  cmdShortScan
			  }
			  else if (action == "W") {
			    if (usingShip != null) { 
				    if (cmd.length == 1) {
				    	usingShip.setVelocity(0,0)
				    	""
				    }
				    else if (cmd.length > 1){
				    	val params = cmd.substring(1,cmd.length)
				    	val dir = params.substring(0,1)
				    	var amt = 0
				    	try {
				    		amt = Integer.parseInt(params.substring(1,params.length))
				    	}
				    	catch {
				    	  case ex:java.lang.Exception => amt = 0
				    	}
				    	val a = amt / 1000.0
         
				    	val v = usingShip.getVelocity()

				    	if (dir == "U") {
				    	  v.setDeltaX(-a)
				    	}
				    	else if (dir == "D") {
				    	  v.setDeltaX(a)
				    	}
				    	else if (dir == "L") {
				    	  v.setDeltaY(-a)
				    	}
				    	else if (dir == "R") {
				    	  v.setDeltaY(a)
				    	}
				    	usingShip.setVelocity(v)
				    	usingShip.setAcceleration(0,0)
				    	""
				    }
			    }
			    else {
			    	showShipsOrLogonMsg
			    } 
			  }
			  else {
				  cmdShortScan
			  }
		  }
    }
 
 	def run = {
		var date = new java.util.Date 
		var cmd = readLine().trim().toUpperCase
		while (cmd != "Q") {
		  var newdate = new java.util.Date
		  val interval = newdate.getTime - date.getTime
		  this.universe.timeMarchesOn(interval.toDouble)
		  println("-------------------------------------------------------------")
		  println(response(cmd))
		  date = new java.util.Date
		  cmd = readLine().trim().toUpperCase
		}
	}
	newUniverse
	run
	
}
