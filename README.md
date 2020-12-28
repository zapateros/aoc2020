# aoc2020
Advent of Code 2020

It was the third year in a row I participated in AoC. I really had a blast solving the problems, as they were very diverse this year (in contrast to last year). 
This year I tried not to look at any information that could directly or indirectly help me (like the forum or even the leaderboard, as solving times could give things away). 

I got some commentary that my code is not very readable this year. That's very true as that was not my purpose. This year I started trying to solve the problems
in the most elegant way possible; conceptually, not readable. I didn't spend energy on variable names or comments. When the problems got a bit harder and my agenda filled up, my 
new goal was to just solve the problems as quick and dirty as possible so the solutions got less readable over time I guess.

Day 19 was the first one I didn't really knew how to solve it. As I thought bruteforcing was absolutely no option, I tried to really understand the problem. I tried many
approaches but I just couldn't find the right one. This took me so long I got a bit behind and I skipped both 19 and 20 for later. In the end I just tried bruteforcing 
19 anyway and it turned out it was easily possible. And when I read through part 2 I finally understood the problem and in the end I really liked this one. 

Day 23 was the only one I didn't solve in R. Just like day 19, when I couldn't find a fast solution I turned to bruteforcing. However, this method in R would take a million
years so I used C#. 

This year was (again) a lesson in reading comprehension, as in total I spent hours and hours of debugging while the only mistake was my reading (twice or thrice or so). Especially
day 22, where I didn't read correctly the number of card copied in the recursive game (not all but just n).

For day 21 my grep function didn't really help me, as 'grepping' 'nuts' also counted/extracted/removed 'peanuts'. This cost me about 4 hours of my life I'm not getting back.

I think these lines are exactly summarizing my AoC2020:

- "After I solved it and relaxed a bit, I found the relevant expand.grid function so I rewrote my solution as it is significantly more elegant" ~Day 1
- "There is probably a regex oneliner but regex doesn't like me" ~Day 2
- "..solve it with a numeric series and a vector; turned out pretty neat" ~Day 3
- "I couldn't think of a simple trick so I did a recursive. Maybe I'll clean up this code later. Probably not" ~Day 7
- "My solutions are getting uglier and uglier." ~Day 8
- "If they work, they work! Maybe I'll clean them up later. We both know I won't" ~Day 11
- "I'm sorry for this unsightly solution. It's not even fast" ~Day 14
- "I was constantly in a split between trying to find another solution or continue debugging." ~Day 21
- "I bruteforced part 2 in C#. It took about 2 hours. Also I didn't clean the code yet but here is the code that gives you part 2 and part 1 somewhere all along." ~Day 23
- "The hardest part of today was to write every step concisely and think of variable names" ~ Day 20
- "When I didn't completely understood this problem this was the worst one of this year; now I get it, it is one of the best. With this one I also complete AoC2020!" ~ Day 19

Perhaps it doesn't show, but I truly had fun again this year and now I'm done I'm already missing it. On to next year!
