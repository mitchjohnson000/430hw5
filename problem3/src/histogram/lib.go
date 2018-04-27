package histogram

import (
	"math/rand"
	"time"
)

type Hist struct {
	Bins       []int
	NumSamples int
}

func makeRand() *rand.Rand {
	return rand.New(rand.NewSource(time.Now().UTC().UnixNano()))
}

func New(upperBound int) Hist {
	return Hist{
		Bins:       make([]int, upperBound),
		NumSamples: 0,
	}
}

func (h *Hist) merge(other Hist) (err bool) {
	if len(h.Bins) != len(other.Bins){
		return true
	}

	for i:= 0; i< len(h.Bins);i++{
		h.Bins[i] += other.Bins[i]
	}

	return false; 
}

func Make(numSamples int, upperBound int) Hist {
	h := New(upperBound)
	r := makeRand()
	h.NumSamples = numSamples
	for i:= 0; i < numSamples; i++ {
		h.Bins[r.Intn(upperBound)]+= 1
	}
	return h
}

func StartWorker(numSamples int,upperBound int, c chan Hist){
	h:= Make(numSamples,upperBound)
	c <- h
}

// Makes a histogram using the indicated number of workers running in parallel.
func MakeInParallel(numSamples int, upperBound int, numWorkers int) Hist {
	c := make(chan Hist)
	for i:= 0; i < numWorkers;i++ {
		go StartWorker((numSamples/numWorkers),upperBound,c)
	}
	total := New(upperBound)
	total.NumSamples = numSamples

	for i:= 0; i < numWorkers;i++ {
		h := <- c
		total.merge(h)
	}
	return total
}




