When making an electoral estimatation, that is, calculating the % of vote
of each political party, it is quite common to have to weight your data with 
respect to different population parameters, such as gender, educational level, 
rural-urban area, etc.

However, different options are tried before a decision is made. For this reason, 
to make the process painless as possible, I have created two functions to automate 
the process of testing different target populations: a basic weighting approach
(simple_weighting) or using the raking algorithm (which I take from anesrake package).

