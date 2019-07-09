package paasio

import (
	"errors"
	"io"
	"sync"
)

// NewReadCounter creates a new ReadCounter
func NewReadCounter(reader io.Reader) ReadCounter {
	return newWrappedReaderWriter(reader, nil)
}

// NewWriteCounter creates a new WriteCounter
func NewWriteCounter(writer io.Writer) WriteCounter {
	return newWrappedReaderWriter(nil, writer)
}

// NewReadWriteCounter creates a new ReadWriteCounter
func NewReadWriteCounter(readerWriter interface {
	io.Reader
	io.Writer
}) ReadWriteCounter {
	return newWrappedReaderWriter(readerWriter, readerWriter)
}

type wrappedReaderWriter struct {
	readerMutex     *sync.Mutex
	reader          io.Reader
	readerByteCount int64
	readerOpsCount  int
	writerMutex     *sync.Mutex
	writer          io.Writer
	writerByteCount int64
	writerOpsCount  int
}

func newWrappedReaderWriter(reader io.Reader, writer io.Writer) *wrappedReaderWriter {
	return &wrappedReaderWriter{
		readerMutex:     &sync.Mutex{},
		reader:          reader,
		readerByteCount: 0,
		readerOpsCount:  0,
		writerMutex:     &sync.Mutex{},
		writer:          writer,
		writerOpsCount:  0,
		writerByteCount: 0,
	}
}

func (x *wrappedReaderWriter) Read(p []byte) (int, error) {
	if x.reader == nil {
		return -1, errors.New("Inner io.Reader is nil")
	}

	n, err := x.reader.Read(p)

	x.readerMutex.Lock()
	defer x.readerMutex.Unlock()
	x.readerByteCount += int64(n)
	x.readerOpsCount++

	return n, err
}

func (x *wrappedReaderWriter) ReadCount() (int64, int) {
	x.readerMutex.Lock()
	defer x.readerMutex.Unlock()

	return x.readerByteCount, x.readerOpsCount
}

func (x *wrappedReaderWriter) Write(p []byte) (int, error) {
	if x.writer == nil {
		return -1, errors.New("Inner io.Writer is nil")
	}

	n, err := x.writer.Write(p)

	x.writerMutex.Lock()
	defer x.writerMutex.Unlock()
	x.writerByteCount += int64(n)
	x.writerOpsCount++

	return n, err
}

func (x *wrappedReaderWriter) WriteCount() (int64, int) {
	x.writerMutex.Lock()
	defer x.writerMutex.Unlock()

	return x.writerByteCount, x.writerOpsCount
}
