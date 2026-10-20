$title Multi-Period Economic Dispatch with Generator Ramp-Rate Constraints

Sets
    g 'Generators' / g1, g2, g3 /
    t 'Time periods (hours)' / t1*t24 /;

Parameters
    a(g) 'Quadratic cost coefficient ($/MW^2)' / g1 0.12, g2 0.08, g3 0.07 /
    b(g) 'Linear cost coefficient ($/MW)'     / g1 20.0, g2 25.0, g3 30.0 /
    c(g) 'Constant cost coefficient ($)'       / g1 500.0, g2 400.0, g3 200.0 /
    
    Pmin(g) 'Minimum generation capacity (MW)' / g1 20.0, g2 30.0, g3 40.0 /
    Pmax(g) 'Maximum generation capacity (MW)' / g1 150.0, g2 200.0, g3 300.0 /
    
    UR(g)   'Ramp-up limit (MW/hour)'          / g1 30.0, g2 40.0, g3 50.0 /
    DR(g)   'Ramp-down limit (MW/hour)'        / g1 30.0, g2 40.0, g3 50.0 /
    
    P0(g)   'Initial generator output before t1 (MW)' / g1 50.0, g2 80.0, g3 100.0 /;

* Hourly load profile (24-hour daily demand curve)
Parameter Demand(t) 'Hourly system load demand (MW)' /
    t1  220, t2  200, t3  190, t4  180, t5  200, t6  230,
    t7  280, t8  340, t9  400, t10 450, t11 480, t12 490,
    t13 470, t14 460, t15 450, t16 480, t17 520, t18 550,
    t19 530, t20 500, t21 440, t22 380, t23 310, t24 250
/;

Variables
    Z       'Total generation cost over 24 hours ($)'
    P(g,t)  'Power output of unit g at time period t (MW)';

* Enforce individual unit capacity bounds
P.lo(g,t) = Pmin(g);
P.up(g,t) = Pmax(g);

Equations
    cost_obj         'Total daily generation cost'
    power_balance(t) 'Hourly demand balance constraint'
    ramp_up_init(g)  'Ramp-up constraint for the initial time step (t1)'
    ramp_dn_init(g)  'Ramp-down constraint for the initial time step (t1)'
    ramp_up(g,t)     'Ramp-up limit for subsequent periods (t2 to t24)'
    ramp_dn(g,t)     'Ramp-down limit for subsequent periods (t2 to t24)';

* Objective Function: Sum of costs over all generators and time periods
cost_obj.. 
    Z =E= sum((g,t), a(g)*sqr(P(g,t)) + b(g)*P(g,t) + c(g));

* Power Balance Constraint for each hour
power_balance(t).. 
    sum(g, P(g,t)) =E= Demand(t);

* Ramp-Up Limits
ramp_up_init(g).. 
    P(g,'t1') - P0(g) =L= UR(g);

ramp_up(g,t)$(ord(t) > 1).. 
    P(g,t) - P(g,t-1) =L= UR(g);

* Ramp-Down Limits
ramp_dn_init(g).. 
    P0(g) - P(g,'t1') =L= DR(g);

ramp_dn(g,t)$(ord(t) > 1).. 
    P(g,t-1) - P(g,t) =L= DR(g);

* Model Declaration & Solve Statement
Model dynamic_economic_dispatch / all /;

Solve dynamic_economic_dispatch using nlp minimizing Z;

* Display Output Tables in .lst File
Display P.l, Z.l;

* --- AUTOMATED CSV EXPORT FOR PLOTTING ---
File results / dispatch_results.csv /;
results.pw = 32767;
put results;
put "Generator,TimePeriod,PowerOutput_MW" /;
loop((g,t),
    put g.tl, ",", t.tl, ",", P.l(g,t):0:2 /;
);
putclose results;