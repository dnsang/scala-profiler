# Scala Profiler
	
	val result  = Profiler("sum") {
		var sum = 0
		for(x <- 1 to 1000) sum += x
		sum 
	}

# Console View:
+------------------------+-----------------+-----------+----------+-------------+
| Function               | Total Time (ms) | Total Hit | Avg (ms) | Num Pending |
+------------------------+-----------------+-----------+----------+-------------+
| ProfilerTest.sum       | 15              | 2         | 7        | 0           |
| ProfilerTest::asyncSum | 116             | 1         | 116      | 0           |
+------------------------+-----------------+-----------+----------+-------------+

