package main

import (
	"fmt"
	"io"
	"os"
	"time"
)

// ConfigSleeper configurable sleeper
// duration the duration to sleep for
// sleep function to call for sleeping
type ConfigSleeper struct {
	duration time.Duration
	sleep    func(time.Duration)
}

// Sleep sleeps for the configured duration using the configured function
func (c *ConfigSleeper) Sleep() {
	c.sleep(c.duration)
}

// Sleeper interface for stopping a moment
type Sleeper interface {
	Sleep()
}

const finalWord = "Go!"

// Countdown example for mock TTD
func Countdown(out io.Writer, sleeper Sleeper) {
	for i := 3; i > 0; i-- {
		sleeper.Sleep()
		fmt.Fprintln(out, i)
	}
	sleeper.Sleep()
	fmt.Fprint(out, finalWord)
}

func main() {
	Sleeper := &ConfigSleeper{1 * time.Second, time.Sleep}
	Countdown(os.Stdout, Sleeper)
}
