# This file contains functions to manipulate PDF files and create reports using RMarkdown

import os
import shutil
from pathlib import Path
from PyPDF2 import PdfFileReader, PdfFileWriter

def open_file(file_name):
    assert Path(file_name).exists()
    cmd = 'open "%s"' % str(file_name)
    os.system(cmd)


def make_report(template_file,
                report_data_file = None,
                report_python_dir = None,
                output_file = None,
                output_dir = None,
                build_dir = None,
                creation_script = 'make_report.R',
                quiet = True,
                clean = True,
                remove_output = False,
                remove_build = False):
    """
    :param template_file:
    :param report_data_file:
    :param report_python_dir:
    :param output_file:
    :param output_dir:
    :param build_dir:
    :param creation_script:
    :param quiet:
    :param clean:
    :param remove_output:
    :param remove_build:
    :return:
    """

    # check required files
    template_file = Path(template_file)
    assert template_file.exists()

    creation_script = Path(creation_script).with_suffix('.R')
    assert creation_script.exists()

    if report_python_dir is not None:
        report_python_dir = Path(report_python_dir)
        assert report_python_dir.is_dir() and report_python_dir.exists()


    if report_data_file is not None:
        report_data_file = Path(report_data_file)
        assert report_data_file.exists()

    # output file and directory
    if output_file is None:
        output_file = template_file.with_suffix('.pdf')
    else:
        output_file = Path(output_file)

    if output_dir is None:
        output_dir = output_file.parent
    else:
        output_dir = Path(output_dir)

    output_dir.mkdir(exist_ok = True)

    # remove existing files
    if output_file.exists():
        output_file.unlink()

    # build directory
    if build_dir is None:
        build_dir = output_dir / output_file.stem
    else:
        build_dir = Path(build_dir)

    build_dir.mkdir(exist_ok = True)

    # setup build command command
    cmd = ['Rscript "%s" "%s" "%s"' % (creation_script, template_file, report_data_file)]
    cmd.append('--report_python_dir "%s"' % report_python_dir)
    cmd.append('--output_file "%s"' % output_file.name)
    cmd.append('--output_dir "%s"' % output_dir)
    cmd.append('--build_dir "%s"' % build_dir)

    if clean:
        cmd.append('--clean')

    if quiet:
        cmd.append('--quiet')

    cmd.append('--run_pandoc')

    # run command
    cmd = ' '.join(cmd)
    out = os.system(cmd)

    if remove_output:
        output_file.unlink()

    if remove_build:
        shutil.rmtree(build_dir, ignore_errors = True)

    return output_file



# PDF Manipulation
def merge_pdfs(pdf_files, merged_file):
    """
    :param pdf_files:a
    :param merged_file:
    :return:
    """
    pdf_writer = PdfFileWriter()

    for f in pdf_files:
        if f.exists():
            pdf_reader = PdfFileReader(str(f))
            for page in range(pdf_reader.getNumPages()):
                if pdf_reader.getPage(page).getContents():
                    pdf_writer.addPage(pdf_reader.getPage(page))

    # Write out the merged PDF
    with open(merged_file, 'wb') as out:
        pdf_writer.write(out)

    return merged_file