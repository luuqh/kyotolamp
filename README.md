# KyotoLamp

A Lagrangian model for simulating particles transport using random walks developed by Hung Q. Luu in 2012. It was a generalized version of a radioactivity dispersion model developed by Takuya Kobayashi in 2006.

**Source code** 

The model is developed using FORTRAN from scratch. The source code of KyotoLamp is archived in [subfolder](https://github.com/luuqh/kyotolamp/tree/master/).

- [Core](https://github.com/luuqh/kyotolamp/blob/master/core/): Main source codes of KyotoLamp, being used to simulate Lagrangian transports.
- [Set](https://github.com/luuqh/kyotolamp/blob/master/set/): Supporting codes, being used to generate input data and settings for the main program.
- [Post](https://github.com/luuqh/kyotolamp/blob/master/post/): Supporting codes, being used to process output data.

**Case study** 

The case study is to simulate the water transport out of Hakodate Bay area, a region between Honshu and Hokkaido islands, Japan. Here, Lagrangian particle tracking is used to investigate the interplay of barotropic components of both the Tsugaru Warn Current and Tidal Currents in numerical experiments. The Eulerian structures obtained in those models are used to calculate the Lagrangian movement by tracking water particles released in Hakodate Bay. The new position of a labelled water particle at the time step is calculated from its previous position using random walk. Referred the details of formulate and the application in [our publication](https://github.com/luuqh/kyotolamp/blob/master/paper.pdf).

