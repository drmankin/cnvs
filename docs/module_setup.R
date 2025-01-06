devtools::load_all()

cnvs::canvas_setup()



# Quizzes -----------------------------------------------------------------

quiz_desc <- cnvs::create_quiz_desc(standard_time_limit = 12)

quiz_info <- cnvs::create_quiz_info(module_code = "C8891",
                                    quiz_desc = quiz_desc,
                                    first_unlock_date = "2025/01/27",
                                    unlock_time = 9,
                                    last_lock_date = "2025/04/11",
                                    lock_time = 16,
                                    standard_time_limit = 12)
