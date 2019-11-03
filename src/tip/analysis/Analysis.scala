package tip.analysis

import tip.ast.{ADeclaration, AstNode}

/**
  * Trait for program analyses.
  *
  * @tparam R the type of the analysis result
  **/
trait Analysis[+R] {

  /**
    * Performs the analysis and returns the result.
    */
  def analyze(): R
}

/**
  * Trait for (may-)points-to analyses.
  * Can answer may-points-to and may-alias queries.
  */
trait PointsToAnalysis extends Analysis[Unit] {

  /**
    * Builds the points-to map.
    * For each identifier, the points-to map gives the set of cells the identifier may point to.
    */
  def pointsTo(): Map[ADeclaration, Set[AstNode]]

  /**
    * Returns a function that tells whether two given identifiers may point to the same cell.
    * @return a function that returns true if the identifiers may point to the same cell; false if they definitely do not point to the same cell
    */
  def mayAlias(): (ADeclaration, ADeclaration) => Boolean
}
