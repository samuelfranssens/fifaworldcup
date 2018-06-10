# fifaworldcup

Simulate the World Cup with average odds across bookies.

The 'read data' file gets the odds from Oddschecker.com. 

The games of the first round are simulated with actual scores (instead of just with % win, draw, lose).

The games of the second round are simulated with win % based on the odds of teams winning the whole tournament. This is based on the Bradley-Terry approach described here: https://eeecon.uibk.ac.at/~zeileis/news/fifa2018/ (and this is also where I got the whole idea for this project).
