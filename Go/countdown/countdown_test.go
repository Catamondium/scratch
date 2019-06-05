package main

import (
	"bytes"
	"reflect"
	"testing"
	"time"
)

type SpySleeper struct {
	Calls int
}

func (s *SpySleeper) Sleep() {
	s.Calls++
}

const sleep = "sleep"
const write = "write"

type OperationSpy struct {
	Calls []string
}

func (s *OperationSpy) Sleep() {
	s.Calls = append(s.Calls, sleep)
}

func (s *OperationSpy) Write(p []byte) (n int, err error) {
	s.Calls = append(s.Calls, write)
	return
}

type SpyTime struct {
	durationSlept time.Duration
}

func (s *SpyTime) Sleep(duration time.Duration) {
	s.durationSlept = duration
}

func TestCountdown(t *testing.T) {
	t.Run("prints 3 to Go!", func(t *testing.T) {
		buffer := &bytes.Buffer{}
		spySleeper := &SpySleeper{}

		Countdown(buffer, spySleeper)

		got := buffer.String()
		want := `3
2
1
Go!`

		if got != want {
			t.Errorf("Got '%s' want '%s'", got, want)
		}

		if spySleeper.Calls != 4 {
			t.Errorf("Insufficient calls, want 4, got %d", spySleeper.Calls)
		}
	})

	t.Run("sleep before every print", func(t *testing.T) {
		opSpy := &OperationSpy{}
		Countdown(opSpy, opSpy)

		want := []string{
			sleep,
			write,
			sleep,
			write,
			sleep,
			write,
			sleep,
			write,
		}

		if !reflect.DeepEqual(want, opSpy.Calls) {
			t.Errorf("wanted calls %v got %v", want, opSpy.Calls)
		}
	})
}

func TestConfigSleeper(t *testing.T) {
	sleepTime := 5 * time.Second

	spyTime := &SpyTime{}
	sleeper := ConfigSleeper{sleepTime, spyTime.Sleep}
	sleeper.Sleep()

	if spyTime.durationSlept != sleepTime {
		t.Errorf("Should have slept %v but slept for %v", sleepTime, spyTime.durationSlept)
	}
}
