Udemy downloader
================

*Download Udemy course files for offline use*

**In order for the downloader to work you have to take the course on Udemy**

    cabal install --only-dependencies
    cabal run -- ft2000@mail.ru https://www.udemy.com/official-udemy-instructor-course/
================

This is just a toy project to learn something new about Haskell. Use some libraries that I have not used before. So far it has been a joyful ride! During the development of this project I learned a lot about these libraries:

* http-conduit
* html-conduit
* aeson

It was also my first time with heavy use of **ByteStrings** and **Text**. I was confused a bit in the beginning but in the end it feels quite natural to use all those different string representations. It is also worth mentioning about such library as parallel-io which made it possible to download stuff from Udemy concurrently in a matter of seconds (I think I edited just 2 lines to make it work).

Basic description of how everything works:

1. Sign in to Udemy using POST request
    * save Cookies after successful and use them for all future requests
2. Go the courseUrl and search for courseId which will be used in future API requests
3. Using Udemy API [(https://developers.udemy.com/)](https://developers.udemy.com/ "Udemy API")
    * grab course info
    * grab course curriculum (with full information regarding lectures and assets)
4. For every type of asset (there are quite a few of them) determine what we would like to download (not everything is downloaded at the moment, this can be improved in the future)
5. Download everything we are interested in!
