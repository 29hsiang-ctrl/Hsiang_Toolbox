#!/usr/bin/env python3
"""
Hsiang_Toolbox AutoCAD Toolbar Icon Generator
Creates 32x32 24-bit BMP icons - one per LISP command
"""
from PIL import Image, ImageDraw
import os

DIR = r"C:\Hsiang_Toolbox\Icons"
SZ = 32

# ── Palette ───────────────────────────────────────────────────────────
BG    = (255, 255, 255)   # white background
DARK  = (20,  20,  45)    # near-black
BLUE  = (10,  95, 200)    # dimension tools
DKBLU = (5,   60, 140)    # dark blue
ORANG = (210, 100,  0)    # text / label tools
RED   = (185,  28, 28)    # delete tools
GREEN = (12,  120, 12)    # geometry / shape tools
DKGRN = (8,   80,  8)     # dark green
PURPL = (85,  40, 145)    # layout / xref tools
TEAL  = (0,  115,  95)    # leader tool
LGRAY = (155, 155, 155)   # light gray
MGRAY = (100, 100, 100)   # medium gray
YLLOW = (205, 170,  0)    # gold (padlock)
LTRED = (240, 180, 180)   # light red fill
LTGRN = (170, 220, 170)   # light green fill
LTBLU = (180, 215, 255)   # light blue fill
CYAN  = (0,  165, 185)    # center lines


def canvas():
    img = Image.new("RGB", (SZ, SZ), BG)
    return img, ImageDraw.Draw(img)


def save(name, img):
    path = os.path.join(DIR, f"{name}.bmp")
    img.save(path, "BMP")
    print(f"  OK {name}.bmp  ({SZ}x{SZ} 24bpp)")


def dim_h(d, x1, y, x2, col=BLUE):
    """Horizontal dimension line with ticks + arrowheads."""
    d.line([(x1, y), (x2, y)], fill=col, width=2)
    d.line([(x1, y-3), (x1, y+3)], fill=col, width=1)
    d.line([(x2, y-3), (x2, y+3)], fill=col, width=1)
    d.polygon([(x1, y), (x1+4, y-2), (x1+4, y+2)], fill=col)
    d.polygon([(x2, y), (x2-4, y-2), (x2-4, y+2)], fill=col)


# ─── CT: Change Text ─────────────────────────────────────────────────────────
def make_CT():
    img, d = canvas()
    # Bold "T" glyph (orange)
    d.rectangle([3, 6, 28, 11], fill=ORANG)        # crossbar
    d.rectangle([12, 11, 19, 26], fill=ORANG)       # stem
    # Pencil — diagonal body + tip + eraser
    d.polygon([(22, 18), (28, 12), (30, 14), (24, 20)], fill=MGRAY)   # shaft
    d.polygon([(22, 18), (24, 20), (21, 23), (20, 21)], fill=DARK)    # tip body
    d.polygon([(21, 23), (20, 21), (22, 24)], fill=ORANG)              # tip point
    d.rectangle([28, 11, 30, 13], fill=RED)         # eraser cap
    # Edit underline
    d.rectangle([3, 28, 28, 30], fill=RED)
    save("CT", img)


# ─── DA: Dim Align ───────────────────────────────────────────────────────────
def make_DA():
    img, d = canvas()
    # Three horizontal dimension lines at varying widths
    dim_h(d,  3,  8, 27)   # long
    dim_h(d,  3, 16, 19)   # medium
    dim_h(d,  3, 24, 13)   # short
    # Red vertical "target" alignment line
    d.line([(28, 4), (28, 28)], fill=RED, width=2)
    # Arrows pointing toward target line
    for ay in (16, 24):
        d.polygon([(28, ay), (23, ay-2), (23, ay+2)], fill=RED)
    save("DA", img)


# ─── LL: Auto Label ──────────────────────────────────────────────────────────
def make_LL():
    img, d = canvas()
    # Three label badge boxes stacked (A series)
    for i, (col_fill, col_line) in enumerate(
            [(LGRAY, LGRAY), (ORANG, DARK), (BG, LGRAY)]):
        y0, y1 = 3 + i * 9, 10 + i * 9
        d.rounded_rectangle([2, y0, 16, y1], radius=2,
                             fill=col_fill, outline=col_line, width=2)
    # Letter marks: narrow vertical bar inside each tag (A, B, C symbol)
    d.line([(9, 4), (9, 9)],  fill=BG, width=2)     # tag 1 glyph
    d.line([(8, 6), (10, 6)], fill=BG, width=1)
    d.line([(9, 12), (9, 17)], fill=BG, width=2)    # tag 2 (on orange bg)
    d.line([(8, 14), (10, 14)], fill=BG, width=1)
    d.line([(9, 21), (9, 26)], fill=LGRAY, width=2)  # tag 3
    d.line([(8, 23), (10, 23)], fill=LGRAY, width=1)
    # Arrow sequence indicators
    d.polygon([(19, 11), (16, 9), (16, 13)], fill=DARK)
    d.line([(16, 11), (19, 11)], fill=DARK, width=1)
    d.polygon([(19, 20), (16, 18), (16, 22)], fill=LGRAY)
    d.line([(16, 20), (19, 20)], fill=LGRAY, width=1)
    # Counter showing current number
    d.rounded_rectangle([20, 7, 30, 17], radius=2,
                         fill=ORANG, outline=DARK, width=1)
    d.line([(24, 9), (24, 15)], fill=BG, width=2)   # "B" simplified
    d.arc([23, 9, 29, 13], 270, 90, fill=BG, width=2)
    d.arc([23, 12, 29, 16], 270, 90, fill=BG, width=2)
    # Cursor dot
    d.ellipse([24, 19, 27, 22], fill=ORANG)
    d.ellipse([24, 24, 27, 27], fill=LGRAY)
    save("LL", img)


# ─── LX: Lock Xref ───────────────────────────────────────────────────────────
def make_LX():
    img, d = canvas()
    # Dashed rectangle → external reference symbol
    step = 4
    for x in range(2, 20, step * 2):
        xe = min(x + step, 19)
        d.line([(x, 3), (xe, 3)],   fill=PURPL, width=2)
        d.line([(x, 18), (xe, 18)], fill=PURPL, width=2)
    for y in range(3, 18, step * 2):
        ye = min(y + step, 17)
        d.line([(2, y), (2, ye)],   fill=PURPL, width=2)
        d.line([(19, y), (19, ye)], fill=PURPL, width=2)
    # "Xref" diagonal arrows inside box
    d.line([(5, 6), (9, 15)], fill=PURPL, width=1)
    d.line([(9, 15), (16, 10)], fill=PURPL, width=1)
    # Padlock (gold, lower-right)
    d.rounded_rectangle([18, 19, 30, 30], radius=3,
                         fill=YLLOW, outline=DARK, width=1)
    # Shackle arc
    d.arc([19, 14, 29, 24], 180, 360, fill=DARK, width=3)
    # Keyhole
    d.ellipse([23, 22, 26, 25], fill=DARK)
    d.rectangle([23, 24, 26, 28], fill=DARK)
    save("LX", img)


# ─── CA: Cache All Layouts ───────────────────────────────────────────────────
def make_CA():
    img, d = canvas()
    # Three stacked "page" rectangles
    d.rectangle([2, 5, 17, 18],  fill=(235,235,235), outline=PURPL, width=1)
    d.rectangle([5, 8, 20, 21],  fill=(245,245,245), outline=PURPL, width=1)
    d.rectangle([8, 11, 24, 26], fill=BG, outline=PURPL, width=2)
    # Horizontal "content" lines on front page
    for y in (16, 19, 22):
        d.line([(11, y), (21, y)], fill=LGRAY, width=1)
    # Large green checkmark over front page
    d.line([(10, 20), (14, 25)], fill=GREEN, width=3)
    d.line([(14, 25), (24, 14)], fill=GREEN, width=3)
    save("CA", img)


# ─── SD: Set DimStyle ────────────────────────────────────────────────────────
def make_SD():
    img, d = canvas()
    # Measured object (short vertical lines at both ends)
    d.line([(5, 12), (5, 22)],  fill=DARK, width=1)
    d.line([(27, 12), (27, 22)], fill=DARK, width=1)
    # Dimension line with arrows
    dim_h(d, 5, 24, 27)
    # "Style" selector cursor at top
    d.arc([10, 4, 22, 12], 30, 210, fill=ORANG, width=3)   # top S arc
    d.arc([10, 9, 22, 17], 200, 30, fill=ORANG, width=3)   # bottom S arc
    # Arrow: apply style downward to dim
    d.line([(16, 17), (16, 22)], fill=DARK, width=2)
    d.polygon([(16, 23), (13, 19), (19, 19)], fill=DARK)
    save("SD", img)


# ─── SN: Sequence Number (command: NN) ───────────────────────────────────────
def make_SN():
    img, d = canvas()
    # Three text-entity rectangles
    # Box 1 — done (light gray)
    d.rectangle([1, 3, 11, 10], fill=(230,230,230), outline=LGRAY, width=1)
    d.line([(6, 5), (6, 9)],   fill=LGRAY, width=2)   # digit "1"
    d.line([(4, 5), (6, 4)],   fill=LGRAY, width=1)
    # Box 2 — active (orange)
    d.rectangle([1, 12, 11, 19], fill=ORANG, outline=DARK, width=1)
    d.arc([2, 13, 10, 16], 0, 180, fill=BG, width=2)  # digit "2"
    d.line([(2, 16), (10, 18)], fill=BG, width=1)
    d.line([(2, 18), (10, 18)], fill=BG, width=2)
    # Box 3 — next (white)
    d.rectangle([1, 21, 11, 28], fill=BG, outline=LGRAY, width=1)
    d.arc([2, 22, 10, 25], 0, 180, fill=LGRAY, width=2)  # digit "3"
    d.arc([2, 24, 10, 27], 0, 180, fill=LGRAY, width=2)
    # Cursor arrow pointing at box 2
    d.polygon([(18, 15), (13, 12), (14, 15), (13, 18)], fill=DARK)
    # Number counter box (shows upcoming number)
    d.rounded_rectangle([19, 10, 31, 20], radius=2,
                         fill=BG, outline=ORANG, width=2)
    d.arc([21, 11, 29, 15], 0, 180, fill=ORANG, width=2)
    d.line([(21, 14), (29, 17)], fill=ORANG, width=1)
    d.line([(21, 17), (29, 17)], fill=ORANG, width=2)
    # "+1" badge
    d.ellipse([26, 20, 31, 25], fill=GREEN)
    d.line([(28, 22), (28, 23)], fill=BG, width=1)
    d.line([(27, 22), (29, 22)], fill=BG, width=1)  # "+" sign
    save("SN", img)


# ─── CW: Create Wall / Thickness (command: T) ────────────────────────────────
def make_CW():
    img, d = canvas()
    # Original polyline (thin, gray)
    d.line([(3, 9), (29, 9)],   fill=LGRAY, width=1)
    d.line([(3, 7), (3, 11)],   fill=LGRAY, width=1)
    d.line([(29, 7), (29, 11)], fill=LGRAY, width=1)
    # Filled wall area
    d.rectangle([4, 10, 28, 22], fill=LTGRN)
    # Offset result line (bold green, bottom edge)
    d.line([(3, 22), (29, 22)],  fill=GREEN, width=2)
    # Closing end walls
    d.line([(3, 9), (3, 22)],   fill=GREEN, width=2)
    d.line([(29, 9), (29, 22)], fill=GREEN, width=2)
    # Dimension arrow (thickness indicator)
    mid = 16
    d.line([(mid, 9), (mid, 22)], fill=ORANG, width=1)
    d.polygon([(mid, 9), (mid-2, 13), (mid+2, 13)], fill=ORANG)
    d.polygon([(mid, 22), (mid-2, 18), (mid+2, 18)], fill=ORANG)
    # "T" label (top right corner)
    d.line([(24, 1), (30, 1)], fill=DARK, width=2)
    d.line([(27, 1), (27, 6)], fill=DARK, width=2)
    save("CW", img)


# ─── SLT: Slot ───────────────────────────────────────────────────────────────
def make_SLT():
    img, d = canvas()
    # Slot outline (horizontal stadium / oval shape)
    d.ellipse([1, 8, 31, 24], fill=LTBLU, outline=GREEN, width=3)
    # Center lines (technical drawing convention, cyan)
    d.line([(0, 16), (32, 16)], fill=CYAN, width=1)   # horizontal CL
    d.line([(16,  5), (16, 27)], fill=CYAN, width=1)  # vertical CL
    # Arc-center marks (small filled circles at the two foci)
    for cx in (9, 23):
        d.ellipse([cx-2, 14, cx+2, 18], fill=RED)
    # Diameter annotation (horizontal double-headed arrow)
    d.line([(9, 27), (23, 27)], fill=DARK, width=1)
    d.polygon([(9, 27), (12, 26), (12, 28)], fill=DARK)
    d.polygon([(23, 27), (20, 26), (20, 28)], fill=DARK)
    save("SLT", img)


# ─── DL: Delete Layer ────────────────────────────────────────────────────────
def make_DL():
    img, d = canvas()
    # Layer stack: four horizontal bands
    bands = [(4,  DARK, 2), (10, MGRAY, 1), (16, LGRAY, 1), (22, LGRAY, 1)]
    for y, col, w in bands:
        d.line([(2, y), (18, y)], fill=col, width=w)
        d.line([(2, y-1), (2, y+1)], fill=col, width=1)   # left tick
    # Layer name stub on top layer
    d.line([(4, 3), (16, 3)], fill=DARK, width=1)
    # Red X (delete / purge symbol) — right side, large
    x0, x1, y0, y1 = 20, 30, 3, 29
    d.line([(x0, y0), (x1, y1)], fill=RED, width=3)
    d.line([(x1, y0), (x0, y1)], fill=RED, width=3)
    save("DL", img)


# ─── DC: Delete Cookie (trim + delete enclosed area) ─────────────────────────
def make_DC():
    img, d = canvas()
    # Outer boundary circle
    d.ellipse([2, 2, 30, 30], fill=(245,245,245), outline=DARK, width=2)
    # Inner enclosed area (light red = marked for deletion)
    d.ellipse([7, 7, 25, 25], fill=LTRED, outline=RED, width=2)
    # Red X through inner area
    d.line([(10, 10), (22, 22)], fill=RED, width=2)
    d.line([(22, 10), (10, 22)], fill=RED, width=2)
    # Trim arrow (from outer to inner)
    d.polygon([(16, 3), (14, 7), (18, 7)], fill=DARK)
    save("DC", img)


# ─── DE: Delete Enclosed ─────────────────────────────────────────────────────
def make_DE():
    img, d = canvas()
    # Irregular polygon boundary (the "fence" polyline)
    poly = [(4, 16), (7, 4), (23, 3), (28, 11), (27, 23), (15, 29), (4, 23)]
    d.polygon(poly, fill=(245, 245, 245), outline=DARK, width=2)
    # Objects inside (filled dots)
    for px, py in [(11, 12), (18, 13), (11, 20), (19, 21), (14, 17)]:
        d.ellipse([px-2, py-2, px+2, py+2], fill=DARK)
    # Red X overlay
    d.line([(6, 6), (26, 26)], fill=RED, width=2)
    d.line([(26, 6), (6, 26)], fill=RED, width=2)
    save("DE", img)


# ─── LD: Text Leader ─────────────────────────────────────────────────────────
def make_LD():
    img, d = canvas()
    # Filled arrowhead (pointing at annotation target, upper-left)
    d.polygon([(3, 4), (12, 9), (7, 14)], fill=TEAL)
    # Leader line: diagonal elbow then horizontal shoulder
    d.line([(9, 12), (16, 19)],  fill=TEAL, width=2)
    d.line([(16, 19), (30, 19)], fill=TEAL, width=2)
    # Text block (two horizontal lines representing text rows)
    d.line([(16, 23), (30, 23)], fill=DARK, width=2)
    d.line([(16, 27), (25, 27)], fill=DARK, width=2)
    save("LD", img)


# ─── SCD: Staggered Continuous Dimension ─────────────────────────────────────
def make_SCD():
    img, d = canvas()
    # Object baseline (bottom reference line)
    d.line([(2, 28), (30, 28)], fill=DARK, width=1)
    # Extension lines from three points
    for x in (3, 14, 28):
        d.line([(x, 28), (x, 6)], fill=LGRAY, width=1)
    # Tier 1 (upper): spans points A→B
    d.line([(3, 10), (14, 10)],  fill=BLUE, width=2)
    d.polygon([(3, 10), (7, 8), (7, 12)],    fill=BLUE)
    d.polygon([(14, 10), (10, 8), (10, 12)], fill=BLUE)
    # Tier 2 (lower): spans points B→C (staggered down)
    d.line([(14, 18), (28, 18)], fill=BLUE, width=2)
    d.polygon([(14, 18), (18, 16), (18, 20)], fill=BLUE)
    d.polygon([(28, 18), (24, 16), (24, 20)], fill=BLUE)
    # Vertical step connecting tiers (the "stagger")
    d.line([(14, 10), (14, 18)], fill=DKBLU, width=1)
    save("SCD", img)


# ─── UP: Unfold Plate ────────────────────────────────────────────────────────
def make_UP():
    img, d = canvas()
    # Folded plate: L-shaped cross-section (left half)
    fold = [(2, 10), (2, 27), (11, 27), (11, 19), (8, 19), (8, 10)]
    d.polygon(fold, fill=LTGRN, outline=GREEN, width=2)
    # Fold / bend line (orange → shows where bending occurred)
    d.line([(8, 10), (8, 19)],  fill=ORANG, width=2)
    d.line([(8, 19), (11, 19)], fill=ORANG, width=2)
    # Transform arrow (→)
    d.line([(12, 18), (18, 18)], fill=DARK, width=2)
    d.polygon([(19, 18), (15, 15), (15, 21)], fill=DARK)
    # Unfolded flat plate (right half)
    d.rectangle([20, 10, 30, 27], fill=LTGRN, outline=GREEN, width=2)
    # Auto-dimension marks (output dim lines)
    d.line([(20, 29), (30, 29)], fill=BLUE, width=1)
    d.line([(20, 27), (20, 30)], fill=BLUE, width=1)
    d.line([(30, 27), (30, 30)], fill=BLUE, width=1)
    save("UP", img)


# ─── main ────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    print(f"Generating Hsiang_Toolbox icons ({SZ}x{SZ} 24-bit BMP) ...")
    make_CT()
    make_DA()
    make_LL()
    make_LX()
    make_CA()
    make_SD()
    make_SN()    # saves as SN.bmp  (command: NN)
    make_CW()    # saves as CW.bmp  (command: T)
    make_SLT()
    make_DL()
    make_DC()
    make_DE()
    make_LD()
    make_SCD()
    make_UP()
    print(f"\nDone — 15 icons saved to {DIR}")
