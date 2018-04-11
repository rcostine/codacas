package org.costine.codacas.system

/**
 * list of help information
 */
class HelpInfo extends Info {
  override def data = List(
			  "Info:",
			  "V - version",
			  "? - this list",
			  "",
			  "Movement:",
			  "W[LR]x[UD]y - move x-speed horizontal, y-speed vertical",
			  "W - stop ship",
			  "",
			  "Weapons:",
			  "TSn - set torp speed (n in space units/sec)",
			  "TRn - set torp range (n in space units)",
			  "TPn - set torp payload (n in fooples delivered)",
			  "TFn - set torp fuel (where n is number of milliseconds)",
			  "T? - show current torp settings",
			  "T[n] - fire torps at angle n, where n is 0-360; \"TG\" shows torp angles",
			  "TG - show rough torp angle chart",
			  "T - fire torps at last angle setting",
			  "P{A..Z}n - phaser ship A..Z with n fooples",
			  "",
			  "Scans:",
			  "F - show fooples", 
			  "S - short scan (ship in the middle)",
			  "G - galactic scan (all stars, torps, ships)",
			  "",
			  "Session management:",
			  "Y - yank (typing any command unyanks)",
			  "R - restart a dead ship",
			  "ctrl-D - disconnect",
			  "",
			  "Communication:",
			  "M[A-Z]message - send a short message to the other ship"
	  )
}
