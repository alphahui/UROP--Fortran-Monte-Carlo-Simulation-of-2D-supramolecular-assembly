module parameters
    
	integer, parameter :: Side=60			!side of the paralleogram

	integer, parameter :: NoOfAtoms=500        !No of atoms to be added to simulation
	integer, parameter :: SizeAsIsland=10
	integer, parameter :: TimeInterval=130, MaxTime=391,CycleRecord=1000	!number of interval for each txt output
	real, parameter :: AtomsAddedPerCycle =0.07, TempIncPerCycle =0.5  
    !how many time will the time pass

	real(kind=8), parameter::Ed=0.500			!diffusion energy
	real(kind=8), parameter::Eb=0.065			!bonding energy
	real(kind=8), parameter::Tc=300   			!temp.

    real(kind=8), parameter::fc=1E12		!constant frequency
	real(kind=8), parameter::kB=8.625E-5		!Boltzmann constant in eV unit
    
    real(kind=8), parameter::lowlimit=1E-12    
	real(kind=8), parameter::uplimit=1-1E-12  !for test the random number
    
    !Default settings:
    logical :: AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved
    

    
    end module