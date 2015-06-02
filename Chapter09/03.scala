io.Source.fromFile("01.txt").mkString.split("""\W+""").filter(_.length > 12).distinct.foreach(println(_))
