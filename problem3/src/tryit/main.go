package main

import (
	"fmt"
	"histogram"
	"time"
)

func main() {
	start := time.Now()

	// h := histogram.Make(
	//   	DefaultNumSamples,
	// 	DefaultUpperBound,
	// )
	
	h := histogram.MakeInParallel(
		DefaultNumSamples,
		DefaultUpperBound,
       DefaultNumWorkers,
	)


	total_samples := 0
	for bin, samples := range h.Bins {
		fmt.Printf("%v : %v\n", bin, samples)
		total_samples += samples
	}
	elapsed := time.Since(start)
	fmt.Printf("Elapsed: %v\n", elapsed)
	fmt.Printf("Total number of samples:  %v\n", total_samples)
}
