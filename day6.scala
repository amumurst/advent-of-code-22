extension (s: String)
  def findMarkerOfLength(length: Int) =
    s.sliding(length).zipWithIndex.collectFirst { case (s, i) if s.distinct.size == length => i + length }
@main def day6a = readFileLine("6").findMarkerOfLength(4).calculate
@main def day6b = readFileLine("6").findMarkerOfLength(14).calculate
