# Portfolio Setup Instructions

Welcome to CPLN5920/MUSA5080! This guide will help you set up your personal portfolio repository for the semester.

## What You're Building

- Your own portfolio repository on GitHub
- A live website showcasing your work
- A place to document your learning journey  

## Example:

Here is one I made: **[Allison's sample portfolio](https://allisonlass.github.io/cpln-5902-student-site-example)** 
  

## Prerequisites

- [ ] A GitHub account ([create one here](https://github.com/join) if needed)
- [ ] Quarto installed on your computer ([download here](https://quarto.org/docs/get-started/))
- [ ] R and RStudio installed

## Step-by-Step Setup

### Step 1: Customize Your Repository

You should already have a repository that you created when you clicked on the link that I sent you.
If not, go to that link again and click the "Use this template" button and then "Create a new repository."
Now let's personalize it.

#### 1.1 Edit Your Site Title
1. Click on the `_quarto.yml` file
2. Click the pencil icon (✏️) to edit
3. Change `"Your Name - CPLN 5920 Portfolio"` to include your actual name
4. Example: `"Jane Smith - CPLN 5920 Portfolio"`
5. Click **"Commit changes"** at the bottom

#### 1.2 Update Your Homepage
1. Click on the `index.qmd` file
2. Click the pencil icon (✏️) to edit
3. Update the **"About Me"** section with your information:
   - Your name and background
   - Your email address
   - Your GitHub username
   - Why you're taking this course
4. Click **"Commit changes"**

#### 1.3  Quarto Render
This step creates a docs folder and an index.html file. You need to make sure
these exist for your website to show up.

1. Click render button in RStudio or run `quarto render` in your command line
2. Check the docs folder for index.html file

### Step 2: Enable GitHub Pages

This step makes your portfolio visible as a live website!

1. **Go to Settings**: Click the "Settings" tab at the top of your repository
2. **Find Pages**: Scroll down and click "Pages" in the left sidebar
3. **Configure Source**: 
   - Source: Select "Deploy from a branch"
   - Branch: Select "main" 
   - Folder: Select "/ docs"
4. **Save**: Click "Save"
5. **Wait**: GitHub will show a message that your site is being built (this takes 1-5 minutes)

### Step 3: Test Your Website

1. **Find Your URL**: After a few minutes, GitHub will show your website URL at the top of the Pages settings
   - It will look like: `https://yourusername.github.io/repository-name`
2. **Visit Your Site**: Click the link to see your live portfolio!
3. **Bookmark It**: Save this URL - you'll submit it to Canvas


## How to Work on Your Portfolio Locally

You can work entirely online, but I don't suggest it. To work on your computer 
and see changes before publishing, you will use either use GitHub Desktop 
or the terminal in RStudio (there are some other ways, too, but these are probably
the easiest). You may be most comfortable with GitHub Desktop at first, but
eventually you will probably find it faster to use the terminal.

In the terminal, this is some relevant code:
**To follow the steps using the bash code, you need to install git (which is different GitHub Desktop) and finish the git client setup**

### Clone Your Repository
```bash
# Replace [your-repo-url] with your actual repository URL. Not your website, but the github repo.
git clone [your-repo-url]
cd [your-repository-name]
```

### Make Changes and Preview
This code renders your qmd file as a html file and saves it in your \docs folder
```bash
# Edit your files using RStudio
# Preview your changes:
quarto render --to html
quarto preview
#Hit Cntrl-C or Cmd-C when you are done to stop the preview

# When ready, save your changes:
git add .
git commit -m "Update portfolio"
git push
```

Your live website will automatically update when you push changes!


## Troubleshooting

### My website isn't showing up
- **Wait longer**: GitHub Pages can take up to 10 minutes to build
- **Check Actions tab**: Look for any red X marks indicating build failures
- **Verify Pages settings**: Make sure you selected "main" branch and "/docs" folder

### I can't edit files
- **Check permissions**: Make sure you're in YOUR repository, not the template
- **Sign in**: Ensure you're signed into GitHub

### My Quarto site won't render
- **Check YAML syntax**: Make sure your `_quarto.yml` file has proper formatting
- **Verify file names**: Files should end in `.qmd` not `.md`
- **Look at error messages**: The Actions tab will show specific error details

### I made a mistake
- **Don't panic!** Every change is tracked in Git
- **See history**: Click the "History" button on any file to see previous versions
- **Revert changes**: You can always go back to a previous version

## Pro Tips

1. **Commit often**: Save your work frequently with descriptive commit messages
2. **Use branches**: For major changes, create a new branch and merge when ready
3. **Preview locally**: Use `quarto preview` to see changes before publishing
4. **Keep it professional**: This portfolio can be shared with future employers!
5. **Document everything**: Good documentation is as important as good analysis

## Additional Resources

- [Quarto Documentation](https://quarto.org/docs/)
- [GitHub Docs](https://docs.github.com/)
- [Markdown Guide](https://www.markdownguide.org/)
- [Git Tutorial](https://learngitbranching.js.org/)


## Checklist

Before submitting, make sure you've:
- [ ] Customized `_quarto.yml` with your name
- [ ] Updated `index.qmd` with your information
- [ ] Enabled GitHub Pages
- [ ] Verified your website loads correctly
- [ ] Submitted your URL to Canvas

---

**Need help? Don't struggle alone - you are strongly encouraged to work with your <br>
classmates. You can also reach out during office hours or in class!**
