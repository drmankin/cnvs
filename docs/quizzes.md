
# Quizzes

This guide will walk you through the process of working with Canvas quizzes using `cnvs`.

**NOTE**: I really mean this - **DO NOT USE FUCKING AI FOR THIS**.
Not only is it shit generally, but it will hallucinate endpoints, arguments etc. that make sense but don't exist.
[USE THE CANVAS API DOCUMENTATION](https://developerdocs.instructure.com/services/canvas/resources) and also preferably YOUR OWN BRAIN.

## Setup

### Establish connection to Canvas

To get set up, load `cnvs` and send your access token and domain to Canvas.

```r
library(cnvs)

cnvs::canvas_setup()
```

### Module ID

Next, the key piece of information you will need is the internal module ID number.
This number is the one that you can see in the module URL: `https://canvas.youruni.co.uk/courses/THIS_NUMBER`

You can retrieve this number with the `cnvs::get_module_id()` function in a number of ways.
The first is if your module has module codes.
Now, apologies for this, but the `cnvs` regex is set up to work with Sussex module codes, particularly for Psychology.
However, it will also search via module name:

```r
module_id <- cnvs::get_module_id("Underwater Basketweaving 101")
```

If you have multiple modules with the same name (for instance, if you teach the same module each year), it will automatically look for the one in the current academic year.
The cutoff for "current" is September 1st (more or less arbitrarily), but you can specify different years:

```r
module_id <- cnvs::get_module_id("Underwater Basketweaving 101", academic_year = "23/24")
```

I *strongly* recommend you store this number as `module_id`, as that is what the subsequent API code will expect.

NOTE: Since this number will be used down the line for constructing API queries, it will NOT tolerate combinations of search term and year that result in a vector longer than 1.
If you want a vector of multiple IDs, or just want to have a look at all the modules you have access to, use `cnvs::get_module_list()` to return a tibble of all available modules.
The module ID number is stored in the first column of this tibble, `id`.

## General Workflow

Not to teach grandma to suck eggs, but just in case, here's a general idea of how it'll go working with the API.
(Feel free to skip this bit if you like!)

Overall, there's an iterative structure to API queries.
If you only want to ever *retrieve* existing information on Canvas, this is going to be a cycle of `GET` queries.
This is because **everything** on Canvas (and I do mean EVERYTHING) has an ID number associated with it.
The cyclical nature of these `GET` queries mainly involves retrieving some info; using the response to find the ID number of the particular thing you want; using that ID to construct another `GET` query; using the response to find the ID number of the next thing you want; and so on.

This is also the case even if you are creating things on Canvas with `POST` or `PUT`.
That's because when you `POST` something to Canvas (like create a new quiz), Canvas generates a new quiz ID for it.
In order to then work with that quiz (like change its settings, or add a new question to it), you first have to `GET` that quiz ID number to be able to generate the appropriate `POST`/`PUT` query.

For quizzes specifically, this might look like:

- Create the quiz itself
- `GET` the quiz ID for that quiz
- Create a question 

Right, let's give it a try!


## Single Quizzes

**Need to debug?** See the [Canvas API documentation for creating quizzes](https://developerdocs.instructure.com/services/canvas/resources/quizzes)!

### Creating a Quiz

To create a basic quiz with nothing in it, the minimum to provide is 1) the `module_id` for the module within which the quiz will be created, and 2) a quiz title.
Let's have a look:

```r
cnvs::create_quiz(module_id, title = "This is a test!")
```

To see your shiny new quiz, navigate to your Canvas site > your module (i.e. the one that has this `module_id`) > Quizzes.
If all's gone well, you should see an unpublished quiz called "This is a test!" (or whatever you called it).

If you click on this quiz, you'll see that in the absence of further info, Canvas has made quite a few assumptions about what this quiz should look like.
For instance, it's not put a time limit, has let students view results immediately, etc.; and there's no description, etc.
So, let's update the quiz to the settings we want.

### Updating a Quiz

In order to update a quiz, we need to tell Canvas which quiz we want to update.
We *can't* use `create_quiz()` for this - that will *only* send a `POST` query to create another new (different) quiz.
Instead, we'll need to use `cnvs::update_quiz()`.

We don't have the `quiz_id` at the moment, because Canvas generated it randomly when it `POST`ed the quiz.
We could look it up in the URL: `https://canvas.youruni.ac.uk/courses/module_id/quizzes/THIS_NUMBER` or, more sensibly, we could use the `cnvs::get_quiz_id()` function to find and store that number.
However, if we just want to make a quick change to the quiz, the `update_quiz()` function will use either a `quiz_id` or a `search_term` to find the relevant quiz.
Here, we can provide `search_term` - either a simple string or regex - to identify the quiz by title.

To make things easy, the function also is designed to be used for a single update task at a time.
You could short-circut this by using the `"other"` task type and `other_args` argument to change multiple things at once.
For now, though, let's just add a description.

```r
cnvs::update_quizzes(
  module_id = module_id,
  search_term = "This is a test!",
  task = "update_description",
  description = "This is a test quiz. I'm going to find out if I can add a description to it."
)
```

Refresh your quiz and check the description box - you should find your shiny new description there.

This function assumes that you've been sensible and added most of the settings for your quiz when you created it, so you'd mainly use the update function to change things that you would need to change later anyway (like publishing it or changing the access code).
So, when developing your workflow, consider deciding what quiz settings you want ahead of time, and implementing them at the create stage wherever possible!

## Creating Multiple Quizzes

Now if you happen to be one of my colleagues (hello!) or just in the same boat as me re: quizzes (hello again, but with more sympathy), you may want to create a whole bunch of very similar quizzes all at once.
At Sussex, we have weekly "worksheet" quizzes covering the lecture and tutorial material, and specific results from a weekly practice worksheet.
Furthermore, we typically have two weeks of term where the quizzes are optional (i.e. for practice only); the rest are marked.
Let's have a look at the process of creating these "worksheet" quizzes with all the relevant bells and whistles.

### Create Quiz Descriptions

To begin, we will generate quiz descriptions by *type* of quiz.
Just run the function on its own to have a look at what it produces.

```r
description <- cnvs::create_quiz_desc()
```

The output will contain two elements, `$practice` and `$marked`.
To be honest, this function is for me, so I've hard-coded it with my own quiz rules.
If you need something different, you can always generate another `description` object yourself; it just needs to contain two elements with the same names (`$practice` and `$marked`).

We also have "ChallengRs", which are optional extra tasks that are a bit harder, but definitely achievable, for each week.
This function also generates ChallengR descriptions by changing the default:

```r
description <- cnvs::create_quiz_desc(challengr = TRUE)
```

### Create Quiz Metadata

Next up, we're going to create a tibble of information about each quiz to pass on to Canvas.
As before, you can either give it the module code, or Canvas module ID; personally, I'd suggest you get in the habit of just storing and always using the `module_id`, but you do you.
If you created and stored a different description above, you can input it here.
However, if you leave it blank, it will just run `cnvs::create_quiz_desc()` with the defaults, so you can skip the step above if you're happy with that!

From there, we're providing information about ea

```r
quiz_info <- cnvs::create_quiz_info(
  module_id = module_id,
  description = description,
  first_unlock_date = "2026/02/02",
  unlock_time = 9,
  last_lock_date = "2026/05/01",
  lock_time = 20,
  mopup_day = "Sunday",
  quiz_type = "assignment",
  make_practice = c(1, 7),
  break_weeks = 9:11
)
```
