# sendgridr 0.6.3

# sendgridr 0.6.2

* Fix email_chk return TRUE for `a+1@mail.com`.

# sendgridr 0.6.1

* Fix typo on dynamic-templates docs.

# sendgridr 0.6.0

* Add apikey param on `auth_set()` function for non-interactive environment.
* Add envvar `SENDGRID_API`. When on load package with envvar `SENDGRID_API`, use value as api key.

# sendgridr 0.5.1

* Rebuild document with roxygen2 7.2.1

# sendgridr 0.5.0

* api key manage with keyring pacakge.

# sendgridr 0.4.4

* Fix test for Windows.

# sendgridr 0.4.3

* Remove fs package for check file exist and lint other deps.

# sendgridr 0.4.2

# sendgridr 0.4.1

* Add embed_rmd() function using emayili package(0.7.0).

# sendgridr 0.4.0

* Refactor Dynamic Template related functions.
* Add dynamic_Template() function.
* Add template_id validation and force options to bypass.
* Remove unused dependency packages.

# sendgridr 0.3.4

* Fix print function for sg_mail class.
* Remove unused dependency packages.

# sendgridr 0.3.2

* Fix url on description file.

# sendgridr 0.3.1

* Update Docs Return value for dynamic_template_data() and template_id() function.

# sendgridr 0.3.0

* Add dynamic_template_data() and template_id() function with @aedobbyn and tested @CurtisPetersen.

# sendgridr 0.2.4

* Add Docs for cran.

# sendgridr 0.2.2

* Fix DESC for cran.

# sendgridr 0.2.1

* Fix attachments with json add duplicates. [10](https://github.com/mrchypark/sendgridr/issues/10)
* Remove content_id. [9](https://github.com/mrchypark/sendgridr/issues/9)

# sendgridr 0.1.0

* Add content_id on attachments.
* Add auto-setting mime type.

# sendgridr 0.0.3

* `attachments()` function check path param is set right file.

# sendgridr 0.0.2

* Set print function for sg_mail class.

# sendgridr 0.0.1

* Minimal version launched.

# sendgridr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
