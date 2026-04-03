We are submitting this as a paper to the Journal of Open Source Software.

Here are the Submission Guidelines:
  
# Submitting a paper to JOSS

If you've already developed a fully featured research code, released it under an [OSI-approved license](https://opensource.org/licenses), and written good documentation and tests, then we expect that it should take perhaps an hour or two to prepare and submit your paper to JOSS.
But please read these instructions carefully for a streamlined submission.

## Submission requirements

- The software must be open source as per the [OSI definition](https://opensource.org/osd).
- The software must be hosted at a location where users can browse the source code files, open issues, and propose code changes without manual approval of (or payment for) accounts
- The software must have an **obvious** research application.
- You must be a major contributor to the software you are submitting, and have a GitHub account to participate in the review process.
- Your paper must not focus on new research results accomplished with the software.
- Your paper (`paper.md` and BibTeX files, plus any figures) must be hosted in a Git-based repository together with your software.
- The paper may be in a short-lived branch which is never merged with the default, although if you do this, make sure this branch is _created_ from the default so that it also includes the source code of your submission.

In addition, the software associated with your submission must:

- Be stored in a repository that can be cloned without registration.
- Be stored in a repository where the software source files are browsable online without registration.
- Have an issue tracker that is readable without registration.
- Permit individuals to create issues/file tickets against your repository.

### What we mean by research software

JOSS publishes articles about research software. This definition includes software that: solves complex modeling problems in a scientific context (physics, mathematics, biology, medicine, social science, neuroscience, engineering); supports the functioning of research instruments or the execution of research experiments; extracts knowledge from large data sets; offers a mathematical library, or similar. While useful for many areas of research, pre-trained machine learning models and notebooks are not in-scope for JOSS. 

### Scope and significance

JOSS publishes articles about software that demonstrates clear research impact or credible scholarly significance. Your software should represent a meaningful contribution to the research community rather than being a one-off tool for a single analysis. Whether or not you use AI assistance in your development, submissions should demonstrate the irreplaceable human contributions: problem framing, key design decisions, thoughtful architectural choices, and practices that make software usable and sustainable for others.

Some factors that may be considered by editors and reviewers when evaluating scope and significance include:

- **Research impact:** Evidence of publications or analyses using the software, external adopters or integrations, or credible near-term significance demonstrated through comparative benchmarks and reproducible reference materials.
- **Design thinking:** Meaningful architectural decisions and trade-offs considered. We particularly value work that builds upon or extends existing software ecosystems rather than reinventing solutions where quality alternatives already exist.
- **Open development practices:** Sustained development over time with evidence of collaborative effort, public development history, comprehensive testing, clear documentation, and pathways for community contribution.
- Whether a potential user can easily install, understand, and test the software. (If your software is new, please be sure a colleague has tried it.)
- Whether the software is sufficiently useful that it is _likely to be cited_ by other researchers in your domain.

Projects developed privately are not eligible until there is a public record of open development: at least six months of public history prior to submission, with evidence of releases, public issues and pull requests. A history of contributions and engagement from individuals beyond the original team, across organizations, is especially welcome, though not essential. We particularly value projects that show evidence of open development practices from early stages, rather than private development followed by public release.

In addition, JOSS requires that software should be feature-complete (i.e., no half-baked solutions), packaged appropriately according to common community standards for the programming language being used (e.g., [Python](https://packaging.python.org), [R](https://r-pkgs.org/index.html)), and designed for maintainable extension (not one-off modifications of existing tools). "Minor utility" packages, including "thin" API clients, and single-function packages are not acceptable.

### Pre-review screening criteria

Before entering review, all submissions are evaluated against the following gates. A submission that fails any one of these will receive a desk rejection, but can be resubmitted once the gaps are addressed.

#### Must meet (all required)

**1. Sufficient public development history**

The repository must have been public for more than six months prior to submission, with active development spanning that period. A repository made public immediately before submission, or one showing development concentrated into a few days or weeks,
will not be accepted. We run automated checks on commit distribution — a repo dump is not a history.

**2. Demonstrated research impact**

There must be evidence that the software is being used for research — at minimum by the developers themselves, and ideally by others. Acceptable signals include: references in published papers or preprints, DOIs linking to the software, documented adoption by other research groups, or clear integration into research workflows.

Aspirational statements about future use are not sufficient.

**3. Good open source practices**

The repository must show active use of open-source workflows. For multi-author projects this means evidence of issues, pull requests, and public discussion. For single-author projects, this bar can be met more broadly — but *multiple* indicators must be present at submission time: a meaningful public commit history over time, tagged releases or a changelog, tests and CI, clear documentation, a CONTRIBUTING file, and stated support or governance expectations. A solo project that is otherwise clearly open and well-maintained will not be rejected solely for lacking a PR workflow. However, a single-author project with *none* of these signals is not ready.

**4. Iterative development over time**

The development history must show ongoing iteration, not a single burst of commits. We look for evidence that the software has been refined through use and feedback over time. A repository where all significant work was added in a concentrated window is a signal that the project was not developed iteratively.

#### Strong positive signal (not a gate, but counts in your favour)

**Community engagement beyond the authors:** non-author issues, pull requests, or discussions are a strong signal of a healthy open project. Submissions with this kind of engagement are well-positioned for review.

#### If your submission doesn't meet these criteria yet

A desk rejection on these grounds may be a "not yet," rather than a "never." If your submission is otherwise solid, we will attempt to write a rejection notice that tells you specifically what is missing. Fix the gaps, continue developing openly, and resubmit in six months or more.

#### AI Usage Policy

**Author use:** The use of generative AI is permitted for most aspects of a JOSS submission (e.g., software creation and review, generating documentation, assisting with paper authoring), however all such use must be disclosed in an "AI usage disclosure" statement which includes:

- **Tool use:** The tools/models used (and versions) and where they were used (code, paper text, docs).
- **The nature and scope of assistance:** e.g., code generation, refactoring, test scaffolding, copy-editing, drafting.
- **Confirmation of review:** Authors must assert that human authors reviewed, edited, validated all AI-assisted outputs and made the core design decisions.

AI is not allowed for conversational interactions between authors and editors or reviewers unless it is being used for translation purposes.

Authors remain fully responsible for the accuracy, originality, licensing, and ethical/legal compliance of all submitted materials. Failure to provide a complete and accurate disclosure of AI usage may be considered an ethical breach. Consequences can include desk rejection, mandatory revisions, and post-publication correction or withdrawal. In cases of intentional misrepresentation or non-disclosure, JOSS reserves the right to notify the authors' institutions, funders, and/or relevant professional or scholarly societies in accordance with standard research-integrity practices.

**Reviewer use:** Reviewers may use generative AI tools to assist with non-substantive tasks (e.g., grammar checks of their review text, contextualizing public materials and code snippets) and must disclose this briefly at the end of their review.

- **Accountability:** The reviewer – not the AI – owns the evaluation. All judgements, recommendations, and technical claims must be the reviewer's and verified by them.
- **Human-only judgements:** When reviewing JOSS submissions (code or papers), all evaluative decisions – scoring, accept/reject recommendations, and assessments of originality, novelty, correctness, significance, and policy/ethics compliance – must be made by the human reviewer. AI tools may assist with analysis, but they must not render or determine any verdicts.

#### A note on web-based software

Many web-based research tools are out of scope for JOSS due to a lack of modularity and challenges testing and maintaining the code. Web-based tools may be considered 'in scope' for JOSS, provided that they meet one or both of the following criteria: 1) they are built around and expose a 'core library' through a web-based experience (e.g., R/[Shiny](https://www.rstudio.com/products/shiny/) applications) or 2) the web application demonstrates a high-level of rigor with respect to domain modeling and testing (e.g., adopts and implements a design pattern such as [MVC](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller) using a framework such as [Django](https://www.djangoproject.com/)).

Similar to other categories of submission to JOSS, it's essential that any web-based tool can be tested easily by reviewers locally (i.e., on their local machine). 

### Co-publication of science, methods, and software

Sometimes authors prepare a JOSS publication alongside a contribution describing a science application, details of algorithm development, and/or methods assessment. In this circumstance, JOSS considers submissions for which the implementation of the software itself reflects a substantial scientific effort. This may be represented by the design of the software, the implementation of the algorithms, creation of tutorials, or any other aspect of the software. We ask that authors indicate whether related publications (published, in review, or nearing submission) exist as part of submitting to JOSS.

#### Other venues for reviewing and publishing software packages

Authors wishing to publish software deemed out of scope for JOSS have a few options available to them:

- Follow [GitHub's guide](https://guides.github.com/activities/citable-code/) on how to create a permanent archive and DOI for your software. This DOI can then be used by others to cite your work.
- Enquire whether your software might be considered by communities such as [rOpenSci](https://ropensci.org) and [pyOpenSci](https://pyopensci.org).

### Should I write my own software or contribute to an existing package?

While we are happy to review submissions in standalone repositories, we also review submissions that are significant contributions made to existing packages. It is often better to have an integrated library or package of methods than a large number of single-method packages.

## Policies

**Disclosure:** All authors must disclose any potential conflicts of interest related to the research in their manuscript, including financial, personal, or professional relationships that may affect their objectivity. This includes any financial relationships, such as employment, consultancies, honoraria, stock ownership, or other financial interests that may be relevant to the research.

**Acknowledgement:** Authors should acknowledge all sources of financial support for the work and include a statement indicating whether or not the sponsor had any involvement in it.

**Review process:** Editors and reviewers must be informed of any potential conflicts of interest before reviewing the manuscript to ensure unbiased evaluation of the research.

**Compliance:** Authors who fail to comply with the COI policy may have their manuscript rejected or retracted if a conflict is discovered after publication.

**Review and Update:** This COI policy will be reviewed and updated regularly to ensure it remains relevant and effective.

### Conflict of Interest policy for authors

An author conflict of interest (COI) arises when an author has financial, personal, or other interests that may influence their research or the interpretation of its results. In order to maintain the integrity of the work published in JOSS, we require that authors disclose any potential conflicts of interest at submission time.

### Preprint Policy

Authors are welcome to submit their papers to a preprint server ([arXiv](https://arxiv.org/), [bioRxiv](https://www.biorxiv.org/), [SocArXiv](https://socopen.org/), [PsyArXiv](https://psyarxiv.com/) etc.) at any point before, during, or after the submission and review process.

Submission to a preprint server is _not_ considered a previous publication.

### Authorship

Purely financial (such as being named on an award) and organizational (such as general supervision of a research group) contributions are not considered sufficient for co-authorship of JOSS submissions, but active project direction and other forms of non-code contributions are. The authors themselves assume responsibility for deciding who should be credited with co-authorship, and co-authors must always agree to be listed. In addition, co-authors agree to be accountable for all aspects of the work, and to notify JOSS if any retraction or correction of mistakes are needed after publication.

### Submissions using proprietary languages/development environments

We strongly prefer software that doesn't rely upon proprietary (paid for) development environments/programming languages. However, provided _your submission meets our requirements_ (including having a valid open source license) then we will consider your submission for review. Should your submission be accepted for review, we may ask you, the submitting author, to help us find reviewers who already have the required development environment installed.

## Submission Process

Before you submit, you should:

- Make your software available in an open repository (GitHub, Bitbucket, etc.) and include an [OSI approved open source license](https://opensource.org/licenses).
- Make sure that the software complies with the [JOSS review criteria](review_criteria). In particular, your software should be full-featured, well-documented, and contain procedures (such as automated tests) for checking correctness.
- Write a short paper in Markdown format using `paper.md` as file name, including a title, summary, author names, affiliations, and key references. See our [example paper](example_paper) to follow the correct format.
- (Optional) create a metadata file describing your software and include it in your repository. We provide [a script](https://gist.github.com/arfon/478b2ed49e11f984d6fb) that automates the generation of this metadata.

### Submitting your paper

Submission is as simple as:

- Filling in the [short submission form](http://joss.theoj.org/papers/new)
- Waiting for the managing editor to start a pre-review issue over in the JOSS reviews repository: https://github.com/openjournals/joss-reviews

### No submission fees

There are no fees for submitting or publishing in JOSS. You can read more about our [cost and sustainability model](http://joss.theoj.org/about#costs).

## Review Process

After submission:

- An Associate Editor-in-Chief will carry out an initial check of your submission, and proceed to assign a handling editor.
- The handling editor will assign two or more JOSS reviewers, and the review will be carried out in the [JOSS reviews repository](https://github.com/openjournals/joss-reviews).
- Authors will respond to reviewer-raised issues (if any are raised) on the submission repository's issue tracker. Reviewer and editor contributions, like any other contributions, should be acknowledged in the repository. 
- **JOSS reviews are iterative and conversational in nature.** Reviewers are encouraged to post comments/questions/suggestions in the review thread as they arise, and authors are expected to respond in a timely fashion.
- **Authors are expected to respond to reviewer feedback promptly.** We generally ask that authors respond to reviewer comments and questions within 2 weeks, and complete requested changes within 4-6 weeks, matching the commitment we ask of reviewers, unless otherwise negotiated with the editor. If you need more time (for example, if the changes requested are particularly substantial), please communicate this to the reviewers and editor on the review thread. Prolonged unresponsiveness may result in the paper being rejected due to lack of engagement.
- Authors and reviewers are asked to be patient when waiting for a response from an editor. Please allow a week for an editor to respond to a question before prompting them for further action.
- Upon successful completion of the review, authors will make a tagged release of the software, and deposit a copy of the repository with a data-archiving service such as [Zenodo](https://zenodo.org/) or [figshare](https://figshare.com/), get a DOI for the archive, and update the review issue thread with the version number and DOI.
- After we assign a DOI for your accepted JOSS paper, its metadata is deposited with CrossRef and listed on the JOSS website.
- The review issue will be closed, and an automatic post from [@JOSS at Mastodon](https://fosstodon.org/@joss) will announce it!

If you want to learn more details about the review process, take a look at the [reviewer guidelines](reviewer_guidelines).

## Confidential requests

Please write admin@theoj.org with confidential matters such as retraction requests, report of misconduct, and retroactive author name changes.

In the event of a name change request, the DOI will remain unchanged, and the paper will be updated without the publication of a correction notice. Please note that because JOSS submissions are managed publicly, updates to papers are visible in the public record (e.g., in the [JOSS papers repository](https://github.com/openjournals/joss-papers) commit history).

JOSS will also update Crossref metadata.

Here is the JOSS Paper Format:

# JOSS Paper Format

Submitted articles must use Markdown and must provide a metadata section at the beginning of the article. Format metadata using YAML, a human-friendly data serialization language (The Official YAML Web Site, 2022). The information provided is included in the title and sidebar of the generated PDF. 

---

## What should my paper contain?

```{important}
Begin your paper with a summary of the high-level functionality of your software for a non-specialist reader. Avoid jargon in this section.
```

JOSS welcomes submissions from broadly diverse research areas. For this reason, we require that authors include in the paper some sentences that explain the software functionality and domain of use to a non-specialist reader. We also require that authors explain the research applications of the software. The paper should be between 750-1750 words. Authors submitting papers significantly longer than 1750 words may be asked to reduce the length of their paper.

Your paper must include the following **required sections**:

- **Summary**: A description of the high-level functionality and purpose of the software for a diverse, *non-specialist audience*.
- **Statement of need**: A section that clearly illustrates the research purpose of the software and places it in the context of related work. This should clearly state what problems the software is designed to solve, who the target audience is, and its relation to other work.
- **State of the field**: A description of how this software compares to other commonly-used packages in the research area. If related tools exist, provide a clear "build vs. contribute" justification explaining your unique scholarly contribution and why existing alternatives are insufficient.
- **Software design**: An explanation of the trade-offs you weighed, the design/architecture you chose, and why it matters for your research application. This should demonstrate meaningful design thinking beyond a superficial code structure description.
- **Research impact statement**: Evidence of realized impact (publications, external use, integrations) or credible near-term significance (benchmarks, reproducible materials, community-readiness signals). The evidence should be compelling and specific, not aspirational.
- **AI usage disclosure**: Transparent disclosure of any use of generative AI in the software creation, documentation, or paper authoring. If no AI tools were used, state this explicitly. If AI tools were used, describe how they were used and how the quality and correctness of AI-generated content was verified.

Your paper must also include:

- A list of the authors of the software and their affiliations, using the correct format (see the example below).
- A list of key references, including to other software addressing related needs. Note that the references should include full names of venues, e.g., journals and conferences, not abbreviations only understood in the context of a specific discipline.
- Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it.
- Acknowledgement of any financial support.

As this list shows, JOSS papers are expected to contain a limited set of metadata (see example below), and sections for Summary, Statement of need, State of the field, Software design, Research impact statement, AI usage disclosure, Acknowledgements, and References. You can look at an [example accepted paper](example_paper). Given this format, a "full length" paper is not permitted, and software documentation such as API (Application Programming Interface) functionality should not be in the paper and instead should be outlined in the software documentation.

```{important}
Your paper will be reviewed by two or more reviewers in a public GitHub issue. Take a look at the [review checklist](review_checklist) and  [review criteria](review_criteria) to better understand how your submission will be reviewed.
```

## Article metadata

(author-names)=
### Names

Providing an author name is straight-forward: just set the `name` attribute. However, sometimes more control over the name is required.

#### Name parts

There are many ways to describe the parts of names; we support the following:

- given names,
- surname,
- dropping particle,
- non-dropping particle,
- and suffix.

We use a heuristic to parse names into these components. This parsing may produce the wrong result, in which case it is necessary to provide the relevant parts explicitly.

The respective field names are

- `given-names` (aliases: `given`, `first`, `firstname`)
- `surname` (aliases: `family`)
- `suffix`

The full display name will be constructed from these parts, unless the `name` attribute is given as well.

#### Particles

It's usually enough to place particles like "van", "von", "della", etc. at the end of the given name or at the beginning of the surname, depending on the details of how the name is used.

- `dropping-particle`
- `non-dropping-particle`

#### Literal names

The automatic construction of the full name from parts is geared towards common Western names. It may therefore be necessary sometimes to provide the display name explicitly. This is possible by setting the `literal` field, e.g., `literal: Tachibana Taki`. This feature should only be used as a last resort. <!-- e.g., `literal: 宮水 三葉`. -->

#### Example

```yaml
authors:
  - name: John Doe
    affiliation: '1'

  - given-names: Ludwig
    dropping-particle: van
    surname: Beethoven
    affiliation: '3'

  # not recommended, but common aliases can be used for name parts.
  - given: Louis
    non-dropping-particle: de
    family: Broglie
    affiliation: '4'
```

The name parts can also be collected under the author's `name`:

``` yaml
authors:
  - name:
      given-names: Kari
      surname: Nordmann
```

  <!-- - name: -->
  <!--     literal: 立花 瀧 -->
  <!--     given-names: 瀧 -->
  <!--     surname: 立花 -->

### Date

The date must be specified using the following format: `%e %B %Y` 

``` yaml
date: 9 October 2024
```

## Affiliations

Each affiliation requires an `index` and `name`.

Optionally, the Research Organization Registry (ROR) identifier for the top-level
organization can be annotated with the `ror` key. Note that ROR does not include
departments in its [scope](https://ror.org/registry/#scope-and-criteria-for-inclusion),
so ROR annotations are typically made to the top-level organization.

```yaml
authors:
  - name: Albert Krewinkel
    affiliation: "1, 2, 3"

affiliations:
  - index: 1
    name: Laboratório Nacional de Luz Síncrotron, Brazil
  - index: 2
    name: Gadjah Mada University, Indonesia
  - index: 3
    name: Technische Universitaet Hamburg, Germany
    ror: "04bs1pb34"
```

## Internal references

The goal of Open Journals is to provide authors with a seamless and pleasant writing experience. Since Markdown has no default mechanism to handle document internal references, known as “cross-references”, Open Journals supports a limited set of LaTex commands. In brief, elements that were marked with `\label` and can be referenced with `\ref` and `\autoref`.

[Open Journals]: https://theoj.org

    ![View of coastal dunes in a nature reserve on Sylt, an island in
    the North Sea. Sylt (Danish: *Slid*) is Germany's northernmost
    island.](sylt.jpg){#sylt width="100%"}

### Tables and figures

Tables and figures can be referenced if they are given a *label* in the caption. In pure Markdown, this can be done by adding an empty span `[]{label="floatlabel"}` to the caption. LaTeX syntax is supported as well: `\label{floatlabel}`.

Link to a float element, i.e., a table or figure, with `\ref{identifier}` or `\autoref{identifier}`, where `identifier` must be defined in the float's caption. The former command results in just the float's number, while the latter inserts the type and number of the referenced float. E.g., in this document `\autoref{proglangs}` yields "\autoref{proglangs}", while `\ref{proglangs}` gives "\ref{proglangs}".

: Comparison of programming languages used in the publishing tool. []{label="proglangs"}

    | Language | Typing          | Garbage Collected | Evaluation | Created |
    |----------|:---------------:|:-----------------:|------------|---------|
    | Haskell  | static, strong  | yes               | non-strict | 1990    |
    | Lua      | dynamic, strong | yes               | strict     | 1993    |
    | C        | static, weak    | no                | strict     | 1972    |

### Equations

Cross-references to equations work similarly to those for floating elements. The difference is that, since captions are not supported for equations, the label must be included in the equation:

    $$a^n + b^n = c^n \label{fermat}$$

Referencing, however, is identical, with `\autoref{eq:fermat}` resulting in "\autoref{eq:fermat}".

$$a^n + b^n = c^n \label{eq:fermat}$$

Authors who do not wish to include the label directly in the formula can use a Markdown span to add the label:

    [$$a^n + b^n = c^n$$]{label="eq:fermat"}

## Markdown
Markdown is a lightweight markup language used frequently in software development and online environments. Based on email conventions, it was developed in 2004 by John Gruber and Aaron Swartz. 

### Inline markup

The markup in Markdown should be semantic, not presentations. The table below has some basic examples.


| Markup              | Markdown example        | Rendered output       |
|:--------------------|:------------------------|:----------------------|
| emphasis            | `*this*`                | *this*                |
| strong emphasis     | `**that**`              | **that**              |
| strikeout           | `~~not this~~`          | ~~not this~~          |
| subscript           | `H~2~O`                 | H{sub}`2`O            |
| superscript         | `Ca^2+^`                | Ca{sup}`2+`           |
| underline           | `[underline]{.ul}`      | [underline]{.ul}      |
| small caps          | `[Small Caps]{.sc}`     | [Small Caps]{.sc}     |
| inline code         | `` `return 23` ``       | `return 23`           |

### Links

Link syntax is `[link description](targetURL)`. E.g., this link to the [Journal of Open Source Software](https://joss.theoj.org/) is written as \
`[Journal of Open Source Software](https://joss.theoj.org/)`.

Open Journal publications are not limited by the constraints of print publications. We encourage authors to use hyperlinks for websites and other external resources. However, the standard scientific practice of citing the relevant publications should be followed regardless.

### Grid tables

Grid tables are made up of special characters which form the rows and columns, and also change table and style variables.

Complex information can be conveyed by using the following features not found in other table styles:

* spanning columns
* adding footers
* using intra-cellular body elements
* creating multi-row headers

Grid table syntax uses the characters "-", "=", "|", and "+" to represent the table outline:

* Hyphens (-) separate horizontal rows.
* Equals signs (=) produce a header when used to create the row under the header text.
* Equals signs (=) create a footer when used to enclose the last row of the table.
* Vertical bars (|) separate columns and also adjusts the depth of a row. 
* Plus signs (+) indicates a juncture between horizontal and parallel lines.

Note: Inserting a colon (:) at the boundaries of the separator line after the header will change text alignment. If there is no header, insert colons into the top line.

Sample grid table:

    +-------------------+------------+----------+----------+
    | Header 1          | Header 2   | Header 3 | Header 4 |
    |                   |            |          |          |
    +:=================:+:==========:+:========:+:========:+
    | row 1, column 1   | column 2   | column 3 | column 4 |
    +-------------------+------------+----------+----------+
    | row 2             | cells span columns               |
    +-------------------+------------+---------------------+
    | row 3             | cells      | - body              |
    +-------------------+ span rows  | - elements          |
    | row 4             |            | - here              |
    +===================+============+=====================+
    | Footer                                               |
    +===================+============+=====================+

### Figures and images

To create a figure, a captioned image must appear by itself in a paragraph. The Markdown syntax for a figure is a link, preceded by an exclamation point (!) and a description.  
Example:  
`![This description will be the figure caption](path/to/image.png)`

In order to create a figure rather than an image, there must be a description included and the figure syntax must be the only element in the paragraph, i.e., it must be surrounded by blank lines.

Images that are larger than the text area are scaled to fit the page. You can give images an explicit height and/or width, e.g. when adding an image as part of a paragraph. The Markdown `![Nyan cat](nyan-cat.png){height="9pt"}` includes the image saved as `nyan-cat.png` while scaling it to a height of 9 pt.

### Citations

Bibliographic data should be collected in a file `paper.bib`; it should be formatted in the BibLaTeX format, although plain BibTeX is acceptable as well. All major citation managers offer to export these formats.

Cite a bibliography entry by referencing its identifier: `[@upper1974]`
will create the reference "(Upper 1974)". Omit the brackets when
referring to the author as part of a sentence: "For a case study on
writers block, see Upper (1974)." Please refer to the [pandoc
manual](https://pandoc.org/MANUAL.html#extension-citations) for additional
features, including page locators, prefixes, suffixes, and suppression
of author names in citations.

The full citation will display as

> Upper, D. 1974. "The Unsuccessful Self-Treatment of a Case of \"Writer's
> Block\"." *Journal of Applied Behavior Analysis* 7 (3): 497.
> <https://doi.org/10.1901/jaba.1974.7-497a>.

### Mathematical formulæ

Mark equations and other math content with dollar signs ($). Use a single dollar sign ($) for math that will appear directly within the text. Use two dollar signs ($$) when the formula is to be presented centered and on a separate line, in "display" style. The formula itself must be given using TeX syntax.

To give some examples: When discussing a variable *x* or a short formula like

```{math}
\sin \frac{\pi}{2}
```

we would write $x$ and

    $\sin \frac{\pi}{2}$

respectively. However, for more complex formulæ, display style is more appropriate. Writing

    $$\int_{-\infty}^{+\infty} e^{-x^2} \, dx = \sqrt{\pi}$$

will give us

$$\int_{-\infty}^{+\infty} e^{-x^2} \, dx = \sqrt{\pi}$$

### Footnotes

Syntax for footnotes centers around the "caret" character `^`. The symbol is also used as a delimiter for superscript text and thereby mirrors the superscript numbers used to mark a footnote in the final text.

``` markdown
Articles are published under a Creative Commons license[^1].
Software should use an OSI-approved license.

[^1]: An open license that allows reuse.
```

The above example results in the following output:

> ```{eval-rst}
>
> Articles are published under a Creative Commons license [#f1]_. Software should use an OSI-approved license.
>
> .. rubric:: Footnotes
>
> .. [#f1] An open license that allows reuse.
>
> ```

Note: numbers do not have to be sequential, they will be reordered automatically in the publishing step. In fact, the identifier of a note can be any sequence of characters, like `[^marker]`, but may not contain whitespace characters.


### Blocks

The larger components of a document are called "blocks".

#### Headings

Headings are added with `#` followed by a space, where each additional `#` demotes the heading to a level lower in the hierarchy:

```markdown
# Section

## Subsection

### Subsubsection
```

Please start headings on the first level. The maximum supported level is 5, but paper authors are encouraged to limit themselves to headings of the first two or three levels.

##### Deeper nesting

Fourth- and fifth-level subsections – like this one and the following heading – are supported by the system; however, their use is discouraged. Use lists instead of fourth- and fifth-level headings.


### Lists

Bullet lists and numbered lists, a.k.a. enumerations, offer an additional method to present sequential and hierarchical information.

``` markdown
- apples
- citrus fruits
  - lemons
  - oranges
```

- apples
- citrus fruits
  - lemons
  - oranges

Enumerations start with the number of the first item. Using the the first two [laws of thermodynamics](https://en.wikipedia.org/wiki/Laws_of_thermodynamics) as example,

``` markdown
0. If two systems are each in thermal equilibrium with a third, they are
   also in thermal equilibrium with each other.
1. In a process without transfer of matter, the change in internal
   energy, $\Delta U$, of a thermodynamic system is equal to the energy
   gained as heat, $Q$, less the thermodynamic work, $W$, done by the
   system on its surroundings. $$\Delta U = Q - W$$
```

Rendered:

0. If two systems are each in thermal equilibrium with a third, they are also in thermal equilibrium with each other.
1. In a process without transfer of matter, the change in internal energy, $\Delta U$, of a thermodynamic system is equal to the energy gained as heat, $Q$, less the thermodynamic work, $W$, done by the system on its surroundings. $$\Delta U = Q - W$$


## Checking that your paper compiles

JOSS uses Pandoc to compile papers from their Markdown form into a PDF. There are a few different ways you can test that your paper is going to compile properly for JOSS:

### GitHub Action

If you're using GitHub for your repository, you can use the [Open Journals GitHub Action](https://github.com/marketplace/actions/open-journals-pdf-generator) to automatically compile your paper each time you update your repository.

The PDF is available via the Actions tab in your project and click on the latest workflow run. The zip archive file (including the `paper.pdf`) is listed in the run's Artifacts section.

### Docker

If you have Docker installed on your local machine, you can use the same Docker Image to compile a draft of your paper locally. In the example below, the `paper.md` file is in the `paper` directory. Upon successful execution of the command, the `paper.pdf` file will be created in the same location as the `paper.md` file:

```text
docker run --rm \
    --volume $PWD/paper:/data \
    --user $(id -u):$(id -g) \
    --env JOURNAL=joss \
    openjournals/inara
```

### Locally

The materials for the `inara` container image above are themselves open source and available in [its own repository](https://github.com/openjournals/inara). You can clone that repository and run the `inara` script locally with `make` after installing the necessary dependencies, which can be inferred from the [`Dockerfile`](https://github.com/openjournals/inara/blob/main/Dockerfile).

## Behind the scenes

Readers may wonder about the reasons behind some of the choices made for paper writing. Most often, the decisions were driven by radical pragmatism. For example, Markdown is not only nearly ubiquitous in the realms of software, but it can also be converted into many different output formats. The archiving standard for scientific articles is JATS, and the most popular publishing format is PDF. Open Journals has built its pipeline based on [pandoc](https://pandoc.org), a universal document converter that can produce both of these publishing formats as well as many more.

A common method for PDF generation is to go via LaTeX. However, support for tagging -- a requirement for accessible PDFs -- is not readily available for LaTeX. The current method used ConTeXt, to produce tagged PDF/A-3.

You can see an EXAMPLE PAPER in example_paper.md.


Here are the JOSS Policies:

# JOSS Policies

## Animal research policy

In the exceptional case a JOSS submission contains original data on animal research, the corresponding author must confirm that the data was collected in accordance with the latest guidelines and applicable regulations. The manuscript must include complete reporting of the data, including the ways that the study was approved and relevant details of the sample. Authors are required to report either the [ARRIVE](https://arriveguidelines.org/) or [PREPARE](https://doi.org/10.1177/0023677217724823) guidelines for animal research in the submission. 

We recommend that authors replace, reduce, and refine animal research as promoted by the [N3RS](https://www.nc3rs.org.uk/).

## Data sharing policy

If any original data analysis results are provided within the submitted work, such results have to be entirely reproducible by reviewers, including accessing the data.

Submissions are, by definition, contained within one or multiple repositories, which we require authors to archive before we accept the submission. Any data contained within the software is made available accordingly.


## Human participants research policy

In the exceptional case a JOSS submission contains original data on human participants, the study must have been performed in accordance with the [Declaration of Helsinki](https://www.wma.net/policies-post/wma-declaration-of-helsinki-ethical-principles-for-medical-research-involving-human-subjects/). The manuscript must contain all relevant information regarding ethics approval, including but not limited to the responsible ethics committee and reference number. This also applies to ethics exemptions. Informed consent must be collected from all human research participants, and authors are required to state whether this occurred in the manuscript.

## AI usage policy

The Journal of Open Source Software permits the use of generative AI in submissions with mandatory disclosure and human oversight requirements.

### Author AI usage

The use of generative AI is permitted for most aspects of a JOSS submission (e.g., software creation and review, generating documentation, assisting with paper authoring).

**Required disclosure:** All submissions must include an AI usage disclosure section in the paper that clearly states whether generative AI tools were used. This disclosure must include:

1. **Tool identification:** Specify which AI systems and versions were employed, noting exactly where they were applied (code, documentation, manuscript sections).

2. **Scope of assistance:** Describe the nature of support provided—examples include code generation, refactoring assistance, testing scaffolding, editorial review, or manuscript drafting.

3. **Human verification confirmation:** Authors must affirm that human team members thoroughly reviewed, modified, and validated all AI-generated content while making primary architectural and design decisions.

**Prohibited AI interactions:** Conversational use of AI between authors and editors/reviewers is restricted, except for translation support.

**Author accountability:** Submitting authors bear complete responsibility for accuracy, originality, licensing compliance, and ethical/legal standards of submitted materials. Incomplete or inaccurate AI disclosures constitute ethical violations with consequences ranging from desk rejection to institutional notification.

### Reviewer AI usage

Reviewers may employ AI tools for non-substantive tasks (grammar checking, code contextualization) requiring brief disclosure at review conclusion.

**Critical constraint:** All evaluative determinations—scoring, recommendations, originality assessments, and compliance judgments—must reflect human reviewer judgment exclusively.
