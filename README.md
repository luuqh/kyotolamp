# KyotoLamp

A Lagrangian model for simulating particles transport using random walks.

**Source Code** 

The model is developed using C\C++ from scratch. It can be configured to run either on PC or HPC. The source code of KyotoLamp is archived in [subfolder](https://github.com/luuqh/kyotolamp/blob/master/kyotolamp/).

**Case study** 

The case study is to sumulate the water transport out of Hakodate Bay area, a region between Honshu and Hokkaido islands, Japan. Here, Lagrangian particle tracking is used to investigate the interplay of barotropic components of both the Tsugaru Warn Current and Tidal Currents in numerical experiments. The Eulerian structures obtained in those models are used to calculate the Lagrangian movement by tracking water particles released in Hakodate Bay. The new position of a labeled water particle at the time step is calculated from its previous position using random walk. Referred the details of fomulate and the application in [our publication](https://github.com/luuqh/kyotolamp/blob/master/paper.pdf).

