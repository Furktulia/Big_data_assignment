package dataset

import dataset.util.Commit.Commit

import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import scala.math.Ordering.Implicits._

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {

  /** Q23 (4p)
   * For the commits that are accompanied by stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = input.filter(_.stats.isDefined).map(_.stats.get.additions).sum / input.count(_.stats.isDefined)

  /** Q24 (4p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * NB!filename of a file is always defined.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]): (Int, Int) = {
    import java.text.SimpleDateFormat
    import java.util.{Date, TimeZone}
    import scala.collection.mutable

    val hourFormat = new SimpleDateFormat("H")
    hourFormat.setTimeZone(TimeZone.getTimeZone("UTC"))

    val hourCounts = mutable.Map[Int, Int]().withDefaultValue(0)

    input.foreach { commit =>
      // Get the commit time in UTC hour
      val date: Date = commit.commit.committer.date
      val hour: Int = hourFormat.format(date).toInt

      // Count the number of .js files in this commit
      val jsFileCount: Int = commit.files.count { file =>
        file.filename.exists(_.toLowerCase.endsWith(".js"))
      }

      // Accumulate the count for this hour
      hourCounts(hour) += jsFileCount
    }

    if (hourCounts.nonEmpty) {
      val (maxHour, maxCount) = hourCounts.maxBy { case (_, count) => count }
      (maxHour, maxCount)
    } else {
      // No .js files found; return (0, 0) or handle as appropriate
      (0, 0)
    }
  }
  /** Q25 (5p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String): (String, Int) = input.filter(_.url.contains(repo)).groupBy(_.commit.author.name).mapValues(_.size).maxBy(_._2)

  /** Q26 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    def extractRizz(url: String): String = {
      val pattern = """.*/repos/([^/]+/[^/]+)/commits.*""".r // sps starichku za obiesnenie
      url match {
        case pattern(repoName) => repoName // W Rizz
        case _ => "" // L Rizz
      }
    }
    input
      .filter(c => c.commit.committer.date.getYear  == 119)
      .groupBy(c => extractRizz(c.url))
      .filterKeys(_ != "")
      .mapValues(_.size)
  }


  /** Q27 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   * NB!filename of a file is always defined.
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = {
    def extractExtension(filename: String): String = {
      val pattern = """.*\.(.*)""".r
      filename match {
        case pattern(extension) => extension
        case _ => ""
      }
    }
    input.flatMap(_.files.flatMap(_.filename)).map(extractExtension).groupBy(x => x).mapValues(_.size).toList.sortBy(-_._2).take(5)
  }


  /** Q28 (9p)
   *
   * A day has different parts:
   * morning 5 am to 12 pm (noon)
   * afternoon 12 pm to 5 pm.
   * evening 5 pm to 9 pm.
   * night 9 pm to 4 am.
   *
   * Which part of the day was the most productive in terms of commits ?
   * Return a tuple with the part of the day and the number of commits
   *
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   */
  def mostProductivePart(input: List[Commit]): (String, Int) = {
    import java.text.SimpleDateFormat
    import java.util.{Date, TimeZone}
    import scala.collection.mutable

    val hourFormat = new SimpleDateFormat("H")
    hourFormat.setTimeZone(TimeZone.getTimeZone("UTC"))

    val partCounts = mutable.Map[String, Int]().withDefaultValue(0)

    def getPartOfDay(hour: Int): String = hour match {
      case h if h >= 5 && h < 12 => "morning"
      case h if h >= 12 && h < 17 => "afternoon"
      case h if h >= 17 && h < 21 => "evening"
      case _ => "night"
    }

    input.foreach { commit =>
      // Get the commit time in UTC hour
      val date: Date = commit.commit.committer.date
      val hour: Int = hourFormat.format(date).toInt

      // Determine the part of the day
      val partOfDay: String = getPartOfDay(hour)

      // Accumulate the count for this part
      partCounts(partOfDay) += 1
    }

    if (partCounts.nonEmpty) {
      val (maxPart, maxCount) = partCounts.maxBy { case (_, count) => count }
      (maxPart, maxCount)
    } else {
      // No commits found; return ("", 0) or handle as appropriate
      ("", 0)
    }
  }
}
