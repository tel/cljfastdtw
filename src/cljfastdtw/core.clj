(ns cljfastDTW.core
  (:import [com.timeseries TimeSeries TimeSeriesPoint]
           [com.dtw FastDTW DTW]
           [java.util ArrayList]))

(defn make-ts
  "Creates a time-series object from the passed stream. Measurements
  are assumed to be in row-major order, so d specifies how to break
  the steam into new d-dimensional observations."
  [stream d]
  (let [N (count stream)]
    (TimeSeries. (ArrayList. (cons "Time" (map #(str "c" %) (range d))))
                 (ArrayList. (map double (range (/ N d))))
                 (ArrayList. (map #(TimeSeriesPoint. (into-array Double/TYPE %))
                                  (partition d stream))))))

(defn matrix->ts
  "Build a ts from a row-major EJML SimpleMatrix."
  [m]
  (let [n (.numRows m)
        d (.numCols m)]
    (TimeSeries. (ArrayList. (cons "Time" (map #(str "c" %) (range d))))
                 (ArrayList. (map double (range n)))
                 (ArrayList. (map #(TimeSeriesPoint.
                                    (into-array
                                     Double/TYPE
                                     (iterator-seq
                                      (.iterator m true % 0 % (dec d)))))
                                  (range n))))))

(defn ts-dtw-dist
  "Computes the DTW distance between two TimeSeries objects. If radius
  is passed it'll use the FastDTW algorithm."
  [ts1 ts2 & [radius]]
  (if radius
    (FastDTW/getWarpDistBetween ts1 ts2 radius)
    (DTW/getWarpDistBetween ts1 ts2)))

(defn m-dtw-dist
  "Computes the DTW distance between time series stored as a row-major
  EJML SimpleMatrices. Uses FastDTW if radius is given."
  [m1 m2 & [radius]]
  (ts-dtw-distance (matrix->ts m1)
                   (matrix->ts m2)
                   radius))