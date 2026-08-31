import os
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters.img import ImageFormatter

OUT = "/home/claude/proyecto_R/output/figuras"
os.makedirs(OUT, exist_ok=True)

lexer = get_lexer_by_name("r")

def make_shot(nombre_archivo, titulo_barra, codigo, out_path, font_size=15, line_numbers=True):
    formatter = ImageFormatter(
        style="friendly",
        font_name="DejaVu Sans Mono",
        font_size=font_size,
        line_numbers=line_numbers,
        line_number_bg="#eeeeee",
        line_number_fg="#999999",
        line_pad=6,
        image_pad=18,
    )
    tmp = out_path.replace(".png", "_code.png")
    with open(tmp, "wb") as f:
        highlight(codigo, lexer, formatter, f)

    # Agregar una "barra de título" tipo editor con el nombre de archivo,
    # para que se vea como una captura de pantalla real de RStudio/VSCode.
    from PIL import Image, ImageDraw, ImageFont
    code_img = Image.open(tmp)
    bar_h = 34
    w, h = code_img.size
    final = Image.new("RGB", (w, h + bar_h), "#3c3f41")
    draw = ImageDraw.Draw(final)
    # tres puntos de ventana
    for i, color in enumerate(["#ff5f56", "#ffbd2e", "#27c93f"]):
        draw.ellipse([16 + i * 22, 11, 16 + i * 22 + 12, 23], fill=color)
    try:
        font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 14)
    except Exception:
        font = ImageFont.load_default()
    draw.text((100, 9), titulo_barra, fill="#cccccc", font=font)
    final.paste(code_img, (0, bar_h))
    final.save(out_path)
    os.remove(tmp)
    print("OK:", out_path)
